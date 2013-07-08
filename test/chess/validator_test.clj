(ns chess.board-test
  (:use clojure.test
        chess.validator))

(deftest validate-moves
  (testing "generate-path"
    (is (= (generate-path [0 0] [5 5])
           (map #(vector % %) (range 1 5))))
    (is (= (generate-path [3 5] [7 5])
           '([4 5] [5 5] [6 5])))))
