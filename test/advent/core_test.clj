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

(deftest day3_1_horizontal_lines
  (testing "Day 3.1 Horizontal line definitions [Xstart Xend Y]"
    (is (= (day3_1_get_horizontals [[0 0] [8 0] [8 5] [3 5] [3 2]]) [[0 8 0] [8 3 5]]))
    (is (= (day3_1_get_horizontals [[0 0] [0 7] [6 7] [6 3] [2 3]]) [[0 6 7] [6 2 3]]))
    (is (= (day3_1_get_horizontals [[0 0] [0 7] [6 7] [6 3] [2 3] [-3 3] [-3 -1]]) [[0 6 7] [6 2 3] [2 -3 3]]))
    ))

(deftest day3_1_vertical_lines
  (testing "Day 3.1 Vertical line definitions [Ystart Yend X]"
    (is (= (day3_1_get_verticals [[0 0] [8 0] [8 5] [3 5] [3 2]]) [[0 5 8] [5 2 3]]))
    (is (= (day3_1_get_verticals [[0 0] [0 7] [6 7] [6 3] [2 3]]) [[0 7 0] [7 3 6]]))
    (is (= (day3_1_get_verticals [[0 0] [0 7] [6 7] [6 3] [2 3] [-3 3] [-3 -1]]) [[0 7 0] [7 3 6] [3 -1 -3]]))))

(deftest day3_1_manhattan_test
  (testing "Day 3.1 Manhattan distance"
    (is (= (day3_1_manhattan_distance 
            (day3_1_all_intersections_for_both_lines 
             (into [] (map #(name %) '[R75,D30,R83,U83,L12,D49,R71,U7,L72])) 
             (into [] (map #(name %) '[U62,R66,U55,R34,D71,R55,D58,R83])))) 
           159))
    (is (= (day3_1_manhattan_distance 
            (day3_1_all_intersections_for_both_lines 
             (into [] (map #(name %) '[R8,U5,L5,D3])) 
             (into [] (map #(name %) '[U7,R6,D4,L4])))) 
           6))
    ))