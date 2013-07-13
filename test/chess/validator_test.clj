(ns chess.validator-test
  (:use clojure.test
        chess.validator
        chess.board))

(deftest validator-tests
  (testing "generate-path"
    (is (= (generate-path [0 0] [5 5])
           (map #(vector % %) (range 1 5))))
    (is (= (generate-path [3 5] [7 5])
           [[4 5] [5 5] [6 5]])))
  (testing "threatens?"
    (board-reset)
    (board-add 0 0 :rook :black)
    (board-add 0 5 :rook :white)
    (board-add 0 7 :king :white)
    (board-add 2 7 :king :black)
    (is (threatens? [0 0] [0 5]))
    (is (not (threatens? [0 0] [0 7])))
    (is (not (threatens? [0 5] [0 7]))))
  (testing "validate-move"
    (is (not (validate-move [0 0] [0 0])))
    (is (validate-move [0 0] [0 5]))
    (is (not (validate-move [0 7] [1 7]))))
  (testing "get-king-pos"
    (is (= (get-king-pos :white)
           [0 7])))
  (testing "check?"
    (is (not (check? :white)))
    (board-add 7 0 :bishop :black)
    (is (check? :white)))
  (testing "checkmate?"
    (board-reset)
    (board-add 5 2 :king :white)
    (board-add 6 2 :bishop :white)
    (board-add 7 4 :knight :black)
    (board-add 3 5 :king :black)
    (board-add 5 5 :bishop :black)
    (let [board-state (board-get-pieces)]
      (is (check? :black))
      (is (not (checkmate? :black)))
      (is (= (board-get-pieces)
             board-state)))
    (board-reset)
    (board-add 5 0 :rook :white)
    (board-add 6 0 :king :white)
    (board-add 7 0 :queen :black)
    (board-add 5 1 :pawn :white)
    (board-add 6 1 :pawn :white)
    (board-add 7 4 :rook :black)
    (board-add 4 7 :king :black)
    (is (checkmate? :white)))
  (testing "stalemate?"
    (is (not (stalemate? :white)))
    (board-reset)
    (board-add 4 2 :king :white)
    (board-add 3 4 :queen :white)
    (board-add 4 4 :bishop :white)
    (board-add 2 7 :king :black)
    (is (stalemate? :black)))
  (testing "type-moves"
    (init-board)
    (is (validate-move [1 0] [0 2]))
    (is (validate-move [0 1] [0 2]))))