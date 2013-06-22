(ns chess.board)

(def board
  (atom {[:x :y] [:figure :color]}))

(defn board-get [x y]
  (get @board [x y]))

(defn board-remove [x y]
  (swap! board dissoc [x y]))

(defn board-add [x y type color]
  (swap! board assoc [x y] [type color]))
