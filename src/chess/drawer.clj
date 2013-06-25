(ns chess.drawer
  (:use chess.board
        seesaw.core
        seesaw.graphics)
  (:import javax.imageio.ImageIO
           java.io.File))

(def square-size 50)
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
  (doseq [[[x y] [fig col]] @board
          :let [img-name (image-name fig col)
                img (-> img-name File. ImageIO/read)]]
    (.drawImage g img (* x square-size) (* (- 7 y) square-size) nil)))

(defn draw-board []
  (native!)
  (-> (frame :title "Chess"
             :on-close :exit
             :resizable? false
             :content (canvas :size [400 :by 400] :paint painter))
      pack!
      show!)
  nil)
