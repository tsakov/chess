(ns chess.board)

(def board
  "{[:x :y] [:figure :color] & more}"
  (atom {}))

(defn board-get [x y]
  (get @board [x y]))

(defn board-remove [x y]
  (swap! board dissoc [x y]))

(defn board-add [x y type color]
  (swap! board assoc [x y] [type color]))

(defn init-board []
  (doseq [x (range 8)]
    (board-add x 1 :pawn :white)
    (board-add x 6 :pawn :black))
  (doseq [[x figure] (zipmap (range 8)
                             [:rook :knight :bishop :queen
                              :king :bishop :knight :rook])
          [y color] [[0 :white] [7 :black]]]
    (board-add x y figure color)))
