(ns chess.move
  (:use chess.board
        chess.validator))

(def turn (atom :white))
(defn next-turn []
  (swap! turn #(if (= % :white) :black :white)))

(def selected-pos (atom nil))

(declare move)
(declare log-move)

(defn set-selected-pos [x y]
  (let [[figure color] (board-get x y)
        old-pos @selected-pos]
    (if old-pos
        (if (= old-pos [x y])
            (reset! selected-pos nil)
            (if (valid-move (first old-pos) (second old-pos) x y (first (apply board-get old-pos)) (second (apply board-get old-pos)))
                (move old-pos [x y])
                (do
                  (reset! selected-pos nil)
                  "This is not a valid move!")))
        (if (= color @turn)
            (do (reset! selected-pos [x y]) nil)
            "It's not your turn!"))))

(defn move [from to]
  (let [[x y] from
        [x1 y1] to
        [figure color] (board-get x y)]
    (log-move from to)
    (board-remove x y)
    (board-remove x1 y1)
    (board-add x1 y1 figure color)
    (next-turn)
    (reset! selected-pos nil)))

(defn log-move [from to]
  (let [[figure color] (apply board-get from)]
    (println color figure from "->" to)))
