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
    (reset! board {})
    (board-add 0 0 :rook :black)
    (board-add 0 5 :rook :white)
    (board-add 0 7 :king :white)
    (is (threatens? [0 0] [0 5]))
    (is (not (threatens? [0 0] [0 7])))
    (is (not (threatens? [0 5] [0 7]))))
  (testing "validate-move"
    (is (not (validate-move [0 0] [0 0])))
    (is (validate-move [0 0] [0 5])))
  (testing "get-king-pos"
    (is (= (get-king-pos :white)
           [0 7])))
  (testing "check?"
    (is (not (check? :white)))
    (board-add 7 0 :bishop :black)
    (is (check? :white)))
  (testing "checkmate?")
  (testing "stalemate?"))
