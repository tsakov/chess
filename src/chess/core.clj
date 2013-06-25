(ns chess.core
  (:use chess.drawer
        chess.board)
  (:gen-class))

(defn -main
  "Start a new game."
  [& args]
  (init-board)
  (draw-board))
