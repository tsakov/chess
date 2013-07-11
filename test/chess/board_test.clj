(ns chess.board-test
  (:use clojure.test
        chess.board))

(deftest access-board
  (testing "board-get"
    (is (nil? (board-get 0 0))))
  (testing "board-add"
    (board-add 0 0 :king :black)
    (is (= (board-get 0 0)
           {:pos [0 0]
            :type :king
            :color :black})))
  (testing "board-remove"
    (board-remove 0 0)
    (is (nil? (board-get 0 0)))))
