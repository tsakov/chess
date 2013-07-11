(ns chess.drawer
  (:use chess.board
        chess.move
        seesaw.core
        seesaw.graphics)
  (:import javax.imageio.ImageIO
           java.io.File))

(def square-size 50)
(def canvas-size (* square-size 8))
(def board-colors ["#CD853F" "#F5DEB3"])

(defn image-name [type color]
  (str "resources/"
       (first (name color))
       (name type)
       ".png"))

(defn painter [c g]
  (doseq [x (range 8)
          y (range 8)
          :let [bg (board-colors (mod (+ x y) 2))
                x1 (* x square-size)
                y1 (* (- 7 y) square-size)]]
    (draw g
      (rect x1 y1 square-size) (style :background bg))
    (if-let [{:keys [type color]} (board-get x y)]
      (let [img (-> (image-name type color) File. ImageIO/read)]
        (.drawImage g img x1 y1 nil))))
  (if-let [[x y] @selected-pos]
    (let [x1 (* x square-size)
          y1 (* (- 7 y) square-size)]
      (draw g
        (rect x1 y1 square-size) (style :foreground :black :stroke 3)))))

(defn input-listener [e]
  (let [x (int (/ (.getX e) square-size))
        y (- 7 (int (/ (.getY e) square-size)))
        message (set-selected-pos x y)]
    (if message
        (alert e message))
    (-> e .getComponent .repaint)))

(defn draw-board []
  (native!)
  (-> (frame :title "Chess"
             :on-close :exit
             :resizable? false
             :content (canvas :size [canvas-size :by canvas-size]
                              :paint painter
                              :listen [:mouse-pressed input-listener]))
      pack!
      show!)
  nil)
