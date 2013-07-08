(ns chess.validator
  (:use chess.board))

(defmulti valid-move (fn [x y x1 y1 type color] type))
(defn valid-move-dispatch [from to]
  )

(defmulti valid-move valid-move-dispatch)

(defn vacant? [pos]
  (nil? (apply board-get pos)))

(defn sign [n]
  (cond (neg? n) -1
        (zero? n) 0
        :else 1))

(defn generate-path [from to]
  (let [[x y] from
        [x1 y1] to
        dx (sign (- x1 x))
        dy (sign (- y1 y))]
    (map vector
         (if (zero? dx) (repeat x) (range (+ x dx) x1 dx))
         (if (zero? dy) (repeat y) (range (+ y dy) y1 dy)))))

(defmethod valid-move :pawn [x y x1 y1 type color]
  (or (and (= x x1) (= (inc y) y1) (= color :white) (nil? (board-get x1 y1)))
      (and (= x x1) (= (dec y) y1) (= color :black) (nil? (board-get x1 y1)))
      (and (or (= (dec x) x1) (= (inc x) x1)) (= (inc y) y1) (= color :white) (= (second (board-get x1 y1)) :black))
      (and (or (= (dec x) x1) (= (inc x) x1)) (= (dec y) y1) (= color :black) (= (second (board-get x1 y1)) :white))))

(defmethod valid-move :rook [x y x1 y1 type color]
  (and (or (= x x1) (= y y1))
       (every? vacant? (generate-path [x y] [x1 y1]))
       (not= color (second (board-get x1 y1)))))

(defmethod valid-move :knight [x y x1 y1 type color]
  (and (not= color (second (board-get x1 y1)))
       (let [all (for [x [1 2]
                       :let [y (- 3 x)]
                       dx [-1 1]
                       dy [-1 1]]
                   [(* x dx) (* y dy)])]
         ((set (map #(map + [x y] %) all)) [x1 y1]))))

(defmethod valid-move :bishop [x y x1 y1 type color]
  (and (not= color (second (board-get x1 y1)))
       (every? vacant? (generate-path [x y] [x1 y1]))
       (not= y y1)
       (let [k (/ (- x x1) (- y y1))]
         (= 1 (* k k)))))

(defmethod valid-move :queen [x y x1 y1 type color]
  (or (valid-move x y x1 y1 :rook color)
      (valid-move x y x1 y1 :bishop color)))

(defmethod valid-move :king [x y x1 y1 type color]
  (and (not= color (second (board-get x1 y1)))
       (let [dx (- x1 x)
             dy (- y1 y)
             M (max dx dy)
             m (min dx dy)]
         (<= -1 m M 1))))

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
