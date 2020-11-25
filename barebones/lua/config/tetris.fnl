;; in Polywell's fennel repl, run this:
;; (editor.add-mode (require :config.tetris))
;; (tetris)

(local editor (require :polywell))
(local lume (require :polywell.lib.lume))
(local (rows cols size) (values 20 10 25))
(local (x-offset y-offset) (values 10 (+ 10 (* rows size))))

(local shapes {:i [{:x 0 :y 0}  {:x 0 :y 1}  {:x 0 :y 2}  {:x 0 :y 3}]
               :o [{:x 0 :y 0}  {:x 0 :y 1}  {:x 1 :y 0}  {:x 1 :y 1}]
               :t [{:x 0 :y 0}  {:x 0 :y 1}  {:x 0 :y 2}  {:x 1 :y 1}]
               :j [{:x 1 :y 0}  {:x 1 :y 1}  {:x 1 :y 2}  {:x 0 :y 0}]
               :l [{:x 0 :y 0}  {:x 0 :y 1}  {:x 0 :y 2}  {:x 1 :y 0}]
               :s [{:x 1 :y 0}  {:x 1 :y 1}  {:x 0 :y 2}  {:x 0 :y 1}]
               :z [{:x 0 :y 0}  {:x 0 :y 1}  {:x 1 :y 2}  {:x 1 :y 1}]})
(local colors {:i [0 0.5 1] :o [0.8 0.8 0] :t [0.8 0 1]
               :j [0 0 0.8] :l [1 0.5 0] :s [0 0.8 0] :z [1 0 0]})

(var (board piece) (values nil nil))

(fn draw []
  (fn square [x y color]
    (love.graphics.setColor (unpack color))
    (love.graphics.rectangle "fill" (+ x-offset (* x size))
                             (- y-offset (* y size)) size size))

  (love.graphics.setColor 1 1 1)
  (love.graphics.rectangle "line" (+ x-offset size) 10
                           (* cols size) (* rows size))

  (each [y row (ipairs board)]
    (each [x color (pairs row)] (square x y color)))
  (each [_ s (pairs piece.shape)]
    (square (+ piece.x s.x) (+ piece.y s.y) piece.color)))

(fn new-piece []
  (let [shape (lume.randomchoice (lume.keys shapes))]
    {:x (/ cols 2) :y rows :shape (. shapes shape) :color (. colors shape)}))

(fn width [shape] (# (lume.set (lume.map shape :x))))

(fn move [n]
  (fn coll? [s]
    (let [row (. board (+ piece.y s.y))]
      (and row (. row (+ piece.x s.x n)))))
  (when (not (lume.any piece.shape coll?))
    (set piece.x (lume.clamp (+ piece.x n) 1
                             (+ 1 (- cols (width piece.shape)))))))

(fn rotate []
  (let [w (width piece.shape)]
    (each [_ s (pairs piece.shape)]
      (set (s.x s.y) (values s.y (+ (- w s.x) -1)))))
  (move 0))

(fn touching? []
  (fn shape-touch? [s]
    (let [row (. board (+ piece.y s.y -1))]
      (and row (. row (+ piece.x s.x)))))
  (or (= piece.y 1) (lume.any piece.shape shape-touch?)))

(fn drop []
  (when (not (touching?))
    (set piece.y (- piece.y 1))))

(var tick-length 0.4)

(fn land []
  (fn gap? [row]
    (not= (lume.count row (fn [c] (~= c nil))) cols))
  (each [_ s (pairs piece.shape)]
    (tset board (+ piece.y s.y) (or (. board (+ piece.y s.y)) []))
    (tset (. board (+ piece.y s.y)) (+ piece.x s.x) piece.color))
  (each [_ s (lume.ripairs (lume.sort piece.shape "y"))]
    (let [row (. board (+ piece.y s.y))]
      (when (and row (not (gap? row)))
        (table.remove board (+ piece.y s.y))
        (set tick-length (math.max 0.2 (* tick-length 0.97))))))
  (set piece (new-piece)))

(var last-update 0)

(fn update [dt]
  (fn tick []
    (if (and (= piece.y rows) (touching?))
        (editor.activate-mode "tetris-end")
        (not (touching?))
        (set piece.y (- piece.y 1))
        (land)))
  (set last-update (+ last-update dt))
  (when (> last-update tick-length)
    (tick)
    (set last-update (- last-update tick-length))))

(global tetris (fn []
                 (editor.open "*tetris*" "tetris" true)
                 (set (board piece) (values [[]] (new-piece)))))

(fn draw-end []
  (love.graphics.setColor 1 1 1)
  (love.graphics.print "Oh no... press enter to start again." 30 100))

(editor.add-mode {:name "tetris-end"
                  :parent "tetris"
                  :props {:over-draw draw-end}})

{:name "tetris"
 :props {:draw draw :update update}
 :map {"left" (partial move -1)
       "right" (partial move 1)
       "up" rotate
       "down" drop
       "return" tetris ; new game
       "escape" (fn [] (editor.change-buffer (editor.last-buffer)))}
 :ctrl {"q" editor.cmd.quit}}
