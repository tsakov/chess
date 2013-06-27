(ns chess.drawer
  (:use chess.board
        chess.move
        seesaw.core
        seesaw.graphics)
  (:import javax.imageio.ImageIO
           java.io.File))

(def square-size 50)
(def canvas-size (* square-size 8))
(def board-colors ["#F5DEB3" "#CD853F"])

(defn image-name [figure color]
  (str "resources/"
       (first (name color))
       (name figure)
       ".png"))

(defn painter [c g]
  (doseq [x (range 8)
          y (range 8)
          :let [bg (board-colors (mod (+ x y) 2))]]
    (draw g
      (rect (* x square-size) (* y square-size) square-size) (style :background bg)))
  (doseq [[[x y] [fig col]] @board ; FIXME - violated the board abstraction
          :let [img (-> (image-name fig col) File. ImageIO/read)]]
    (.drawImage g img (* x square-size) (* (- 7 y) square-size) nil)))

(defn draw-select-border [g x y]
  (draw g
    (rect (* x square-size) (* (- 7 y) square-size) square-size) (style :foreground :black :stroke 3)))

(def prev-pos ; use move/selected-pos
  "Previously stored position."
  (atom nil))

(defn press-listener [e]
  (let [x (int (/ (.getX e) square-size))
        y (- 7 (int (/ (.getY e) square-size)))]
    (cond
      @prev-pos (do
                  (move @prev-pos [x y])
                  (reset! prev-pos nil)
                  (-> e .getComponent .repaint))
      (board-get x y) (do
                        (reset! prev-pos [x y])
                        (-> e .getComponent .getGraphics (draw-select-border x y))))))

(defn draw-board []
  (native!)
  (-> (frame :title "Chess"
             :on-close :exit
             :resizable? false
             :content (canvas :size [canvas-size :by canvas-size]
                              :paint painter
                              :listen [:mouse-pressed press-listener]))
      pack!
      show!)
  nil)
