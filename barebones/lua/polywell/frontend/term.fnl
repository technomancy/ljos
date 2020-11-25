(local term (require :polywell.lib.plterm))
(local lume (require :polywell.lib.lume))
(local (lfs-ok? lfs) (pcall require :lfs))

(var clipboard "")

(var (prev-termios w h) (values nil 80 25))

(fn get-wh [] (values w h))
(fn set-wh [new-w new-h] (set (w h) (values new-w new-h)))

(fn luajit-raw-term []
  (let [syscall (require :syscall)
        termios (assert (syscall.tcgetattr 1))]
    (set prev-termios (assert (syscall.tcgetattr 1)))
    (termios:makeraw)
    (assert (syscall.tcsetattr 1 "NOW" termios))))

(fn init []
  (match (term.getscrlc)
    (th tw) (set (w h) (values tw th)))
  (when (not (pcall luajit-raw-term))
    (term.setrawmode)))

(fn quit []
  (if prev-termios
      (let [syscall (require :syscall)]
        (assert (syscall.tcsetattr 1 "NOW" prev-termios)))
      (term.setsanemode))
  (os.exit 0))

(fn render-colored-line [x w color line ...]
  (when (and color line)
    (term.color color)
    (term.out (line:sub 1 w))
    (when (< x w)
      (render-colored-line (- w (# line)) w ...))))

(fn render-buffer [b pos w h scroll-line]
  (for [i scroll-line (+ scroll-line h)]
    (term.golc (- i scroll-line -1) 1)
    (match [(. (or b.colored-lines []) i) (. b.lines i)]
      [colored-line] (render-colored-line 1 w (unpack colored-line))
      [nil line] (term.out (line:sub 1 w)))
    (term.color term.colors.default)))

(fn draw-modeline [w h modeline]
  (term.golc h 1)
  (term.color term.colors.reverse)
  (term.out (.. modeline (string.rep " " (- w (# modeline)))))
  (term.color term.colors.normal))

(var (last-scroll last-b last-modeline) nil)

(fn draw [b buffers-where echo-message colors get-prop minibuffer?]
  (let [(w h) (get-wh)
        scroll-line (or b.scroll-line 1)
        modeline (b:modeline)]
    (when (or b.dirty (not= last-scroll scroll-line)
              (not= b last-b) (not= modeline last-modeline))
      (term.clear)
      (each [pos buf (pairs buffers-where)]
        (render-buffer buf pos w h scroll-line))
      (if (= b.path :minibuffer)
          (do (draw-modeline w h (b:render))
              (term.golc h (+ 1 b.point)))
          echo-message
          (draw-modeline w h echo-message)
          (draw-modeline w h modeline))
      (set (last-b last-scroll last-modeline)
           (values b scroll-line modeline)))
    (if (= b.path :minibuffer)
        (term.golc h (+ 1 b.point))
        (term.golc (- b.point-line scroll-line -1) (+ 1 b.point)))))

(fn resolve [path]
  (if (and lfs-ok? (not (path:find "^/")))
      (.. (lfs.currentdir) "/" path)
      path))

(fn ls [path]
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

(fn read [path]
  (table.concat (lume.array (io.lines (resolve path))) "\n"))

(fn write [path contents]
  (when contents
    (with-open [f (assert (io.open (resolve path) "w"))]
      (f:write contents))))

(fn type* [path]
  ;; TODO: fallback when LFS is missing!
  (and lfs-ok? (. (or (lfs.attributes (resolve path)) []) :mode)))

(fn line-height [] 1) ; for scrolling

(fn get-buffer-wh []
  (let [(w h) (get-wh)]
    (values w (- h 1))))

(fn normalize-color [_ color-name]
  (match color-name
    :text term.colors.black
    :keyword term.colors.blue
    :str term.colors.red
    :number term.colors.cyan
    :comment term.colors.green))

(fn unsupported [] (error "Unsupported operation."))

(local mods {:ctrl false :alt false :shift false})

(fn key [readkey]
  (let [init-code (readkey)
        code (if (= 27 init-code) (readkey) init-code)]
    (set mods.ctrl (and (not= code 32) (= (bit.band code 64) 0)))
    (set mods.shift (= (bit.band code 32) 0))
    (set mods.alt (= 27 init-code))
    ;; (log:write (.. code " " (term.keyname code) "\n"))
    (match (term.keyname code)
      "^M" (do (set mods.ctrl false) "return")
      "^J" (do (set mods.ctrl false) "return")
      "^I" (do (set mods.ctrl false) "tab")
      "del" "backspace"
      ;; strip modifiers
      keyname (: (keyname:gsub "^%^" "") :lower))))

(fn mod-down? [modifier] (. mods modifier))

{: init
 : draw
 : quit

 : key

 : get-buffer-wh
 : get-wh
 : set-wh
 : line-height
 : normalize-color
 : mod-down?

 : ls
 : read
 : write
 :type type*

 :get-clipboard (fn [] clipboard)
 :set-clipboard (fn [x] (set clipboard x))

 :resize unsupported
 :scale unsupported
 :set-cursor unsupported
 :set-scale unsupported
 :toggle-fullscreen unsupported
}
