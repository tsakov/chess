(ns chess.validator
  (:use chess.board))

(defmulti valid-move (fn [x y x1 y1 type color] type))
(defn valid-move-dispatch [from to]
  )

(defmulti valid-move valid-move-dispatch)

(defmethod valid-move :pawn [x y x1 y1 type color]
  (or (and (= x x1) (= (inc y) y1) (= color :white) (nil? (board-get x1 y1)))
      (and (= x x1) (= (dec y) y1) (= color :black) (nil? (board-get x1 y1)))
      (and (or (= (dec x) x1) (= (inc x) x1)) (= (inc y) y1) (= color :white) (= (second (board-get x1 y1)) :black))
      (and (or (= (dec x) x1) (= (inc x) x1)) (= (dec y) y1) (= color :black) (= (second (board-get x1 y1)) :white))))

(defmethod valid-move :rook [x y x1 y1 type color]
  (and (or (= x x1) (= y y1))
       (not= color (second (board-get x1 y1)))))

(defmethod valid-move :knight [x y x1 y1 type color]
  true)

(defmethod valid-move :bishop [x y x1 y1 type color]
  true)

(defmethod valid-move :queen [x y x1 y1 type color]
  true)

(defmethod valid-move :king [x y x1 y1 type color]
  true)

(defn threatens? [from to]
  "valid move \" from -> to \" and different colors"
  )

(defn get-king-pos [color]
  (->> (board-get-pieces)
       (filter #(= (second %) [:king color]))
       first
       first))

(defn check? [color]
  ""
  )

(defn checkmate? [color])

(defn stalemate? [color])
