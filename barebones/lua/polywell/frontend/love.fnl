 ;; This file contains all the love2d-specific code; in theory replacing this
 ;; could allow polywell to run on another lua-based framework.

(local lume (require :polywell.lib.lume))
(local (lfs-ok? lfs) (pcall require :lfs))

(var (row-height scroll-rows em w h) nil)
(var (padding buffer-padding) (values 10 0))
(var (canvas fixed-w fixed-h) nil)
(var scale 1)

;; backwards-compatibility with old love2d versions
(local exists? (or love.filesystem.getInfo love.filesystem.exists))

(fn reset-canvas []
  (love.graphics.setCanvas)
  (let [(rw rh) (if (exists? "fullscreen")
                    (love.window.getDesktopDimensions)
                    (love.graphics.getDimensions))]
    (if (and fixed-w fixed-h)
        (do
          (set (w h) (values fixed-w fixed-h))
          (set scale (math.floor (math.min (/ rw w) (/ rh h))))
          (set canvas (love.graphics.newCanvas w h))
          (canvas:setFilter :nearest :nearest))
        (do
          (set (w h) (values (/ rw scale) (/ rh scale)))
          (set canvas (love.graphics.newCanvas w h))
          (canvas:setFilter :nearest :nearest)))))

(fn render-line [ln2 y]
  (if (or (= ln2 "\f\n") (= ln2 "\f"))
      (love.graphics.line 0 (+ y (* 0.5 row-height)) w (+ y (* 0.5 row-height)))
      (love.graphics.print ln2 buffer-padding y)))

(fn normalize-color [c]
  "Support for old-style 255-based colors in love 0.x."
  (if (< 0 love._version_major)
      (let [new []]
        (each [i n (pairs c)]
          (tset new i (/ n 255)))
        new)
      c))

(fn render-buffer [b colors bh focused]
  (let [display-rows (math.floor (/ bh row-height))
        scroll-line (or b.scroll-line 1)]
    (when (or focused (not scroll-rows))
      (set scroll-rows display-rows))
    (each [i line (ipairs (or b.colored-lines b.lines))]
      (when (>= i scroll-line)
        (let [row-y (* row-height (- i scroll-line))]
          (when (>= row-y (- h row-height))
            (lua :break))
          (when (= i b.mark-line)
            (love.graphics.setColor colors.mark)
            (love.graphics.rectangle :line (* b.mark em) row-y em row-height))
          (when (= i b.point-line)
            (love.graphics.setColor colors.point-line)
            (love.graphics.rectangle :fill 0 row-y w row-height)
            (love.graphics.setColor colors.point)
            (love.graphics.rectangle (if focused :fill :line)
                                     (+ buffer-padding (* em b.point))
                                     row-y em row-height))
          (if b.colored-lines
              (love.graphics.setColor (normalize-color [255 255 255]))
              (love.graphics.setColor colors.text))
          (render-line line row-y))))))

(fn draw-scroll-bar [b colors]
   ;; this only gives you an estimate since it uses the amount of
   ;; lines entered rather than the lines drawn, but close enough
  (let [bar-height (math.min 100 (/ (* scroll-rows 100) (# b.lines)))
        bar-height-pixels (/ (* bar-height (- h 10)) 100)
        sx (- w 5)]
    (love.graphics.setColor colors.scroll-bar)
    (if (>= bar-height-pixels (- h 10))
        (love.graphics.line sx 5 sx (- h 5))
        (let [bar-end (/ (* b.point-line 100) (# b.lines))
              bar-end (/ (* (- h 10) bar-end) 100)
              bar-begin (- bar-end bar-height-pixels)]
          (if (< bar-begin 5)
              (love.graphics.line sx 5 sx bar-height-pixels)
              (> bar-end (- h 5))
              (love.graphics.line sx (- (- h 5) bar-height-pixels) sx (- h 5))
              (love.graphics.line sx bar-begin sx bar-end))))))

(fn draw [b buffers-where echo-message colors get-prop minibuffer?]
  (set row-height (: (love.graphics.getFont) "getHeight"))
  (set em (: (love.graphics.getFont) "getWidth" "a"))
  ;; draw background
  (when (not minibuffer?)
    (love.graphics.setColor colors.background)
    (love.graphics.rectangle :fill 0 0 w h))
  (each [pos buf (pairs buffers-where)]
    (when (= (# pos) 0)
      (lume.extend pos [padding 0 (- w padding) (- h row-height)]))
    (let [[x y bw bh] pos]
      (love.graphics.push)
      (love.graphics.translate x y)
      (love.graphics.setScissor x y bw bh)
      (match (get-prop :draw nil buf)
        custom-draw (custom-draw)
        (render-buffer buf colors bh pos.current))
      (love.graphics.pop)
      (love.graphics.setScissor)
      (love.graphics.setColor colors.minibuffer-bg)
      (love.graphics.rectangle :fill 0 (- (- h row-height) 1)
                               w (+ row-height 1))
      (love.graphics.setColor colors.minibuffer-fg)
      (let [minibuffer-h (math.floor (- (- h row-height) 1))]
        (if (= b.path "minibuffer")
            (do
              (love.graphics.print (b:render) padding minibuffer-h)
              (love.graphics.setColor colors.point)
              (love.graphics.rectangle :fill (+ padding (* b.point em))
                                       minibuffer-h em (+ row-height 1)))
            echo-message
            (love.graphics.print echo-message padding minibuffer-h)
            (love.graphics.print (b:modeline) padding minibuffer-h)))
      (when (and (not= b.path "minibuffer") (> (# b.lines) 1))
        (draw-scroll-bar b colors)))))

(fn wrap [f ...]
  (when (not canvas)
    (reset-canvas))
  (love.graphics.setCanvas {:stencil true
                            1 canvas})
  (love.graphics.clear)
  (love.graphics.setColor (normalize-color [255 255 255]))
  (f ...)
  (love.graphics.setCanvas)
  (love.graphics.setColor (normalize-color [255 255 255]))
  (love.graphics.draw canvas 0 0 0 scale scale))

(fn resolve [path]
  (if (and lfs-ok? (not (path:find "^/")))
      (.. (lfs.currentdir) "/" path)
      path))

(fn get-wh [] (canvas:getDimensions))

(local fullscreen-flags {:fullscreen true
                         :fullscreentype "desktop"
                         :resizable false})

(fn lfs-type [path]
  (. (or (lfs.attributes (resolve path)) []) :mode))

(fn love-type [path]
  (. (love.filesystem.getInfo path) :type))

(fn lfs-ls [path]
  (let [parts (lume.split path "/")
        path (if (= "" path)
                 "."
                 (table.concat parts "/"))
        t []]
    (pcall #(each [f (lfs.dir (resolve path))]
              (when (and (not= f ".") (not= f ".."))
                (if (not= path ".")
                    (table.insert t (.. path "/" f))
                    (table.insert t f)))))
    t))

(fn love-ls [path]
  (love.filesystem.getDirectoryItems path))

(fn toggle-fullscreen []
  (when love.window
    (if (exists? "fullscreen")
        (let [dimensions (love.filesystem.read "window")
              (new-w new-h) (: dimensions "match" "(%d+) (%d+)")]
          (set (w h) (values (tonumber new-w) (tonumber new-h)))
          (love.window.setMode w h)
          (love.filesystem.remove "fullscreen")
          (reset-canvas)
          false)
        (let [(dw dh) (love.window.getDesktopDimensions)]
          (love.filesystem.write "window" (.. (* w scale) " " (* h scale)))
          (love.filesystem.write "fullscreen" "true")
          (love.window.setMode dw dh fullscreen-flags)
          (reset-canvas)
          true))))

(local key-down? (or love.keyboard.isScancodeDown love.keyboard.isDown))

(fn mod-down? [modifier]
  ;; ignore keycode since we have access to the actual keyboard state
  (match modifier
    :alt (key-down? "lalt" "ralt")
    :ctrl (key-down? "lctrl" "rctrl" "capslock")
    :shift (key-down? "lshift" "rshift")
    _ (error (.. "Unknown modifier " modifier))))

{:draw draw
 :get-buffer-wh (fn []
                  (let [(_ _ sw sh) (love.graphics.getScissor)]
                    (if (and sw sh)
                        (values sw sh)
                        (get-wh))))
 :get-clipboard (fn [] (and love.window (love.system.getClipboardText)))
 :get-wh get-wh
 :init (fn [] (when (and (exists? "fullscreen") love.window)
                (let [(dw dh) (love.window.getDesktopDimensions)]
                  (love.window.setMode dw dh fullscreen-flags))))
 : mod-down?
 :line-height (fn [] (or row-height 1))
 ;; prefer lfs because it works outside love project dir, but fall back to love
 :ls (if lfs-ok? lfs-ls love-ls)
 :type (or (and lfs-ok? lfs-type) love-type)
 :normalize-color normalize-color
 :quit love.event.quit
 :read (fn [path] (table.concat (lume.array (io.lines (resolve path))) "\n"))
 :resize reset-canvas
 :scale (fn [s]
          (let [s (or s 1)]
            (set scale (math.max 1 (math.min (+ scale s) 4)))
            (reset-canvas)))
 :set-clipboard (fn [contents]
                  (when love.window
                    (love.system.setClipboardText contents)))
 :set-cursor (fn [cursor]
               (if (= (type cursor) "table")
                   (love.mouse.newCursor (.. "assets/" (. cursor 1) ".png")
                                         (. cursor 2) (. cursor 3))
                   cursor (love.mouse.getSystemCursor cursor)))
 :set-scale (fn [s]
              (set scale s)
              (reset-canvas))
 :set-wh (fn [nw nh]
           (set (fixed-w fixed-h) (values nw nh))
           (love.window.setMode fixed-w fixed-h
                                (and (exists? "fullscreen") fullscreen-flags))
           (reset-canvas))
 :toggle-fullscreen toggle-fullscreen
 :wrap wrap
 :write (fn [path contents]
          (when contents
            (with-open [f (assert (io.open (resolve path) "w"))]
              (f:write contents))))}
