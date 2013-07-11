(ns chess.board)

(def board
  "{[:x :y] [:type :color] & more}"
  (atom {}))

(defn board-get [x y]
  (when-let [[type color] (get @board [x y])]
    {:pos [x y]
     :type type
     :color color}))

(defn board-remove [x y]
  (swap! board dissoc [x y])
  nil)

(defn board-add [x y type color]
  (swap! board assoc [x y] [type color])
  nil)

(defn board-get-pieces []
  (for [x (range 8)
        y (range 8)
        :let [piece-info (board-get x y)]
        :when piece-info]
    piece-info))

(defn init-board []
  (doseq [x (range 8)]
    (board-add x 1 :pawn :white)
    (board-add x 6 :pawn :black))
  (doseq [[x type] (zipmap (range 8)
                           [:rook :knight :bishop :queen
                            :king :bishop :knight :rook])
          [y color] [[0 :white] [7 :black]]]
    (board-add x y type color)))
