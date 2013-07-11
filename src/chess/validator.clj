(ns chess.validator
  (:use chess.board))

(defmulti valid-move? (fn [x y x1 y1 type color] type))

(defn free? [pos]
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

(defn free-path? [from to]
  (every? free? (generate-path from to)))

(defmethod valid-move? :pawn [x y x1 y1 type color]
  (or (and (= x x1) (= (inc y) y1) (= color :white) (free? [x1 y1]))
      (and (= x x1) (= (dec y) y1) (= color :black) (free? [x1 y1]))
      (and (or (= (dec x) x1) (= (inc x) x1)) (= (inc y) y1) (= color :white) (= (:color (board-get x1 y1)) :black))
      (and (or (= (dec x) x1) (= (inc x) x1)) (= (dec y) y1) (= color :black) (= (:color (board-get x1 y1)) :white))))

(defmethod valid-move? :rook [x y x1 y1 type color]
  (and (or (= x x1) (= y y1))
       (free-path? [x y] [x1 y1])))

(defmethod valid-move? :knight [x y x1 y1 type color]
  (let [all (for [x [1 2]
                  :let [y (- 3 x)]
                  dx [-1 1]
                  dy [-1 1]]
              [(* x dx) (* y dy)])]
    ((set (map #(map + [x y] %) all)) [x1 y1])))

(defmethod valid-move? :bishop [x y x1 y1 type color]
  (and (free-path? [x y] [x1 y1])
       (not= y y1)
       (let [k (/ (- x x1) (- y y1))]
         (= 1 (* k k)))))

(defmethod valid-move? :queen [x y x1 y1 type color]
  (or (valid-move? x y x1 y1 :rook color)
      (valid-move? x y x1 y1 :bishop color)))

(defmethod valid-move? :king [x y x1 y1 type color]
  (let [dx (- x1 x)
        dy (- y1 y)
        M (max dx dy)
        m (min dx dy)]
    (<= -1 m M 1)))

(defn threatens? [from to]
  (let [[x1 y1] from
        [x2 y2] to
        {type1 :type color1 :color} (apply board-get from)
        {type2 :type color2 :color} (apply board-get to)]
    (and (not (nil? type1))
         (valid-move? x1 y1 x2 y2 type1 color1)
         (= (set [color1 color2]) #{:white :black}))))

(defn get-king-pos [color]
  (->> (board-get-pieces)
       (filter #(= (second %) [:king color]))
       first
       first))

(defn check? [color]
  (let [king-pos (get-king-pos color)
        all-pos (for [x (range 8) y (range 8)] [x y])]
    (some #(threatens? % king-pos) all-pos)))

(defn checkmate? [color])

(defn stalemate? [color])

(defn validate-move [from to]
  (let [[x y] from
        [x1 y1] to
        {:keys [type color]} (board-get x y)
        {color1 :color} (board-get x1 y1)]
    (and (not= from to)
         (not= color color1)
         (valid-move? x y x1 y1 type color))))
