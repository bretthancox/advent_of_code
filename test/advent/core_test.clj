(ns advent.core-test
  (:require [clojure.test :refer :all]
            [advent.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))))

(deftest day2_1_computer_test
  (testing "Day 2.1"
    (is (= (day2_1 [1,0,0,0,99]) [2,0,0,0,99]))
    (is (= (day2_1 [2,3,0,3,99]) [2,3,0,6,99]))
    (is (= (day2_1 [2,4,4,5,99,0]) [2,4,4,5,99,9801]))
    (is (= (day2_1 [1,1,1,4,99,5,6,0,99]) [30,1,1,4,2,5,6,0,99]))
    ))

(deftest day2_1_alarm_prep_test
  (testing "Day 2.1 alarm prep"
    (is (= (day2_prep [1,0,0,0,99] 12 2) [1 12 2 0 99]))
    (is (= (day2_prep [1,1,1,4,99,5,6,0,99] 12 2) [1,12,2,4,99,5,6,0,99]))
    ))

(deftest day2_2_test
  (testing "Day 2.1 noun and verb finder"
    (is (= (day2_2_nounverb [1 5 6 0 99 19690719 1]) [0 5]))
    (is (= (day2_2_result (day2_2_nounverb [1 5 6 0 99 19690719 1])) 5))
    ))

(deftest day3_1_coord_builder
  (testing "Day 3.1 Building coordinates"
    (is (= (day3_1_coordinate_builder ["R8","U5","L5","D3"]) [[0 0] [8 0] [8 5] [3 5] [3 2]]))
    (is (= (day3_1_coordinate_builder ["U7","R6","D4","L4"]) [[0 0] [0 7] [6 7] [6 3] [2 3]]))
    (is (= (day3_1_coordinate_builder ["U7","R6","D4","L4","L5","D4"]) [[0 0] [0 7] [6 7] [6 3] [2 3] [-3 3] [-3 -1]]))
    ))