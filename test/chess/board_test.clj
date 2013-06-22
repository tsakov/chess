(ns chess.board-test
  (:use clojure.test
        chess.board))

(swap! board (fn [_] {[0 0] [:pawn :black]
                      [1 1] [:king :white]}))

(deftest access-board
  (testing "board-get"
    (is (= (board-get 0 0)
           [:pawn :black])))
  (testing "board-remove"
    (is (= (board-remove 0 0)
           {[1 1] [:king :white]})))
  (testing "board-add"
    (is (= (board-add 2 2 :rook :black)
           {[1 1] [:king :white], [2 2] [:rook :black]}))))
