(ns chess.move
  (:use chess.board))

(def selected-pos (atom nil))
(defn get-setlected-pos [] @selected-pos)
(defn set-selected-pos [x y] (reset! selected-pos [x y]))

(defn move [from to]
  (let [[x y] from
        [x1 y1] to
        [figure color] (board-get x y)]
    (println color figure from "->" to)
    (board-remove x y)
    (board-remove x1 y1)
    (board-add x1 y1 figure color)))
