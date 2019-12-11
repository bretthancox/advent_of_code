(ns advent.day3-test
  (:require [clojure.test :refer :all]
            [advent.day3 :refer :all]))


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

(deftest day3_1_intersections
  (testing "Day 3.1 find the intersection coordinates"
    (is (= (day3_1_find_intersection_points [[0 7 0] [7 3 6]] [[0 8 0] [8 3 5]]) [[6 5]]))
    (is (= (day3_1_find_intersection_points [[0 5 8] [5 2 3]] [[0 6 7] [6 2 3]]) [[3 3]]))
    ))

(deftest day3_1_manhattan_test
  (testing "Day 3.1 Manhattan distance"
    (is (= (day3_1_manhattan_distance 
            (into [] (map #(name %) '[R75,D30,R83,U83,L12,D49,R71,U7,L72])) 
            (into [] (map #(name %) '[U62,R66,U55,R34,D71,R55,D58,R83])))
           159))
    (is (= (day3_1_manhattan_distance 
            (into [] (map #(name %) '[R8,U5,L5,D3])) 
            (into [] (map #(name %) '[U7,R6,D4,L4])))
           6))
    ))

(deftest day3_2_crossproduct_test
  (testing "Finding the set of coordinates that terminate in an intersection"
  (is (= (day3_2_crossproduct_output 
          (second 
           (day3_1_all_intersections_for_both_lines 
            (into [] (map #(name %) '[R8,U5,L5,D3]))
            (into [] (map #(name %) '[U7,R6,D4,L4]))))
          (day3_1_coordinate_builder (into [] (map #(name %) '[R8,U5,L5,D3]))))
         [[0 0] [8 0] [8 5] [6 5]]
         ))))

(deftest sum_the_distance_test
  (testing "Summing the distance"
    (is (= (sum_the_distance [[0 0] [8 0] [8 5] [6 5]]) 15))
    (is (= (sum_the_distance [[0 0] [0 7] [6 7] [6 5]]) 15))))

(deftest day3_2_loop_intersects_test
  (testing "day3_2_loop_intersects"
    (is (= (day3_2_loop_intersects
            (into [] (map #(name %) '[R75,D30,R83,U83,L12,D49,R71,U7,L72]))
            (into [] (map #(name %) '[U62,R66,U55,R34,D71,R55,D58,R83])))
           610)
        (= (day3_2_loop_intersects
            (into [] (map #(name %) '[R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51]))
            (into [] (map #(name %) '[U98,R91,D20,R16,D67,R40,U7,R15,U6,R7])))
           410))))