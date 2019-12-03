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
    (is (= (day2_prep [1,0,0,0,99]) [1 12 2 0 99]))
    (is (= (day2_prep [1,1,1,4,99,5,6,0,99]) [1,12,2,4,99,5,6,0,99]))
    ))