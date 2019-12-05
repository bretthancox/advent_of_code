(ns advent.core
  (:gen-class)
  (:require [clojure.string :as str]
            [advent.inputs :refer [day1_masses day2_intcode day3_line1 day3_line2]]))

;; ----------- Day 1 -----------

(defn day1_fuel_calc
  "I do the fuel calculation as defined for day 1"
  [mass]
  (- (int (Math/floor (/ mass 3))) 2))

(defn day1_1
  "I take the mass vector and produce a single fuel requirement"
  [mass_vec]
  (reduce + (map day1_fuel_calc mass_vec)))

(defn day1_2
  "I take the individual masses for each item, work out the fuel requirement for each item, and then recursively calculate the fuel requirement for the fuel"
  [mass]
  (loop [fuel_needed (day1_fuel_calc mass)
         total_fuel 0]
    (if (< fuel_needed 0)
      total_fuel
      (recur (day1_fuel_calc fuel_needed) (+ total_fuel fuel_needed)))))


;; ----------- Day 2.1 -----------

(defn day2_operator
  "Extracts repeated operations from other code"
  [int1 int2 operator]
  (operator int1 int2))

(defn day2_prep
  "Performs the preparation task of replacing the 'noun' and 'verb' per the 2.2 description (aka index 1 and index 2) with new values"
  [no_alarm noun verb]
  (assoc (assoc no_alarm 1 noun) 2 verb))

(defn day2_1
  [intcode]
  (loop [posa 0
         posb 1 
         posc 2 
         posd 3         
         intcode intcode]
    (let [opcode (get intcode posa)]
      (cond
        (= opcode 99) intcode
        (= opcode 1) (recur (+ posa 4) (+ posb 4) (+ posc 4) (+ posd 4)
                            (assoc intcode (get intcode posd) (day2_operator 
                                                               (get intcode (get intcode posb)) 
                                                               (get intcode (get intcode posc)) 
                                                               +)))
        (= opcode 2) (recur (+ posa 4) (+ posb 4) (+ posc 4) (+ posd 4)
                            (assoc intcode (get intcode posd) (day2_operator 
                                                               (get intcode (get intcode posb)) 
                                                               (get intcode (get intcode posc)) 
                                                               *)))
        :else (recur (+ posa 4) (+ posb 4) (+ posc 4) (+ posd 4) intcode)))))

;; ----------- Day 2.2 -----------


(defn day2_2_nounverb
  "I find the noun and verb that produce the desired outcome of 19690720 at index 0 and return them as a vector"
  [intcode max desired_result] 
    (loop [noun 0 verb 0]
      (let [result (day2_1 (day2_prep intcode noun verb))]
        (if (= (get result 0) desired_result)
          [(get result 1) (get result 2)]
          (recur 
           (if (= verb (- max 1)) 
             (inc noun) ;Noun only iterates every full cycle of verb, otherwise it loops without change
             noun)
           (if (= verb (- max 1)) 
             0 ;If a full verb cycle has occurred, reset to 0, otherwise increment
             (inc verb)))
          ))))


(defn day2_2_result
  "I use the noun and verb vector to perform the calculation that produces the final answer for day 2.2"
  [nounverb]
  (+ (* 100 (get nounverb 0)) (get nounverb 1)))



;; ----------- Day 3.1 -----------

(defn day3_1_coordinate_vec_builder_x
  "I construct the coordinates for horizontal lines"
  [operator coordinates next_str]
  (vector (operator (get coordinates 0) ;; X
                    (Integer/parseInt (subs next_str 1)))
          (get coordinates 1))) ;; Y

(defn day3_1_coordinate_vec_builder_y
  "I construct the coordinates for vertical lines"
  [operator coordinates next_str]
  (vector (get coordinates 0) ;; X
          (operator (get coordinates 1)  ;; Y
                    (Integer/parseInt (subs next_str 1)))))

(defn day3_1_coordinate_builder
  "I turn the direction set for each line into coordinates that represent each point the line changes direction"
  [strings]
  (loop [next_str (first strings)
         rest_strings (rest strings)
         coordinates [0 0]
         vec_of_coords [coordinates]]
    (if (= (+ 1 (count strings)) (count vec_of_coords))
      vec_of_coords
      (cond (str/starts-with? next_str "U") (recur (first rest_strings)
                                                   (rest rest_strings)
                                                   (day3_1_coordinate_vec_builder_y + coordinates next_str)
                                                   (conj vec_of_coords (day3_1_coordinate_vec_builder_y + coordinates next_str)))
            (str/starts-with? next_str "D") (recur (first rest_strings)
                                                   (rest rest_strings)
                                                   (day3_1_coordinate_vec_builder_y - coordinates next_str)
                                                   (conj vec_of_coords (day3_1_coordinate_vec_builder_y - coordinates next_str)))
            (str/starts-with? next_str "R") (recur (first rest_strings)
                                                   (rest rest_strings)
                                                   (day3_1_coordinate_vec_builder_x + coordinates next_str)
                                                   (conj vec_of_coords (day3_1_coordinate_vec_builder_x + coordinates next_str)))
            (str/starts-with? next_str "L") (recur (first rest_strings)
                                                   (rest rest_strings)
                                                   (day3_1_coordinate_vec_builder_x - coordinates next_str)
                                                   (conj vec_of_coords (day3_1_coordinate_vec_builder_x - coordinates next_str)))))))


(defn day3_1_get_horizontals
  "I extract the horizontal lines from the coordinate vector. It then produces a three number set for each: [x1 x2 y].
x1 is the start of the line. x2 is the end of the line. y is the constant y-plane. "
  [coordinates]
  (loop [start (first coordinates)
         rest_coords (rest coordinates)
         horizontals []]
    (if (<= (count rest_coords) 0)
      horizontals
      (if 
       (not= 
        (- (first start) (first (first rest_coords))) 0)
        (recur (first rest_coords) (rest rest_coords) (conj horizontals [(first start) (first (first rest_coords)) (second start)]))
        (recur (first rest_coords) (rest rest_coords) horizontals)
        ))))
              

(defn day3_1_get_verticals
  "I extract the vertical lines from the coordinate vector. It then produces a three number set for each: [y1 y2 x].
y1 is the start of the line. y2 is the end of the line. x is the constant x-plane. "
  [coordinates]
  (loop [start (first coordinates)
         rest_coords (rest coordinates)
         verticals []]
    (if (<= (count rest_coords) 0)
      verticals
      (if
       (not=
        (- (second start) (second (first rest_coords))) 0)
        (recur (first rest_coords) (rest rest_coords) (conj verticals [(second start) (second (first rest_coords)) (first start)]))
        (recur (first rest_coords) (rest rest_coords) verticals)))))


(defn day3_1_find_intersection_points
  "For each of the initial lines, I compare the horizontal sub-lines from line A to the vertical sub-lines from line B.
Run again with vertical sub-lines from line A and horizontal sub-lines from line B to get all intersects."
  [vert_lines horiz_lines]
  (loop [vert_loop vert_lines
         horiz_loop horiz_lines ;; bind to a different name so that...
         intersections []]
    (if (empty? vert_loop)
      intersections
      (if (empty? horiz_loop) ;; ...we can reuse the original vector as passed to the function
        (recur (rest vert_loop) horiz_lines intersections) 
        (let [vert_coords (first vert_loop)
              horiz_coords (first horiz_loop)
              vert_x_axis (nth vert_coords 2)
              horiz_y_axis (nth horiz_coords 2)
              ;; A little cheat. The end of a line can have a smaller number than the start
              ;; and catching this situation with code is painful. So I guarantee that the smallest
              ;; x or y value is x1/y1 and the largest is x2/y2. Since the direction of the line
              ;; is irrelevant to determining intersects, this is not a problem.
              vert_y1 (if (> (nth vert_coords 0) (nth vert_coords 1)) (nth vert_coords 1) (nth vert_coords 0))
              vert_y2 (if (> (nth vert_coords 0) (nth vert_coords 1)) (nth vert_coords 0) (nth vert_coords 1))
              horiz_x1 (if (> (nth horiz_coords 0) (nth horiz_coords 1)) (nth horiz_coords 1) (nth horiz_coords 0))
              horiz_x2 (if (> (nth horiz_coords 0) (nth horiz_coords 1)) (nth horiz_coords 0) (nth horiz_coords 1))]
          (if 
           (and
            (and (> horiz_y_axis vert_y1) (< horiz_y_axis vert_y2)) ;; If horizontal y axis is within the y range of the vertical line being assessed
            (and (> vert_x_axis horiz_x1) (< vert_x_axis horiz_x2))) ;; and vertical x axis is within the x range of the horizontal line being assessed
            (recur vert_loop (rest horiz_loop) (conj intersections [vert_x_axis horiz_y_axis])) ;; then the lines intersect and we add it to the vector of intersections.
            (recur vert_loop (rest horiz_loop) intersections))))))) ;; Otherwise, don't update the intersections and move on.


(defn day3_1_all_intersections_for_both_lines
  "I am basically command and control. I walk through each function, passing the result to the next.
Finally merge the intersection points into one vector."
  [line_1 line_2]
  (let [line_1_coordinates (day3_1_coordinate_builder line_1)
        line_2_coordinates (day3_1_coordinate_builder line_2)
        line_1_horizontals (day3_1_get_horizontals line_1_coordinates)
        line_1_verticals (day3_1_get_verticals line_1_coordinates)
        line_2_horizontals (day3_1_get_horizontals line_2_coordinates)
        line_2_verticals (day3_1_get_verticals line_2_coordinates)
        l1v_intersect_l2h (day3_1_find_intersection_points line_1_verticals line_2_horizontals)
        l2v_intersect_l1h (day3_1_find_intersection_points line_2_verticals line_1_horizontals)]
    (loop [intersect_main l1v_intersect_l2h
           intersect_source l2v_intersect_l1h]
      (if (empty? intersect_source)
        intersect_main
        (recur (into intersect_main intersect_source) (rest intersect_source))))))

(defn day3_1_manhattan_distance
  "I take the vector of intersection points and calculate the Manhattan distance. I then compare
that distance to the last recorded distance and, if smaller, store that distance instead. I return the smallest distance."
  [line_1 line_2]
  (loop [intersection_points (day3_1_all_intersections_for_both_lines line_1 line_2)
         intersect (first intersection_points)
         smallest_distance (+ (Math/abs (- 0 (first intersect))) (Math/abs (- 0 (second intersect))))]
    (if (empty? intersection_points)
      smallest_distance
      (recur (rest intersection_points) 
             (first intersection_points) 
             (if (< smallest_distance 
                    (+ (Math/abs (- 0 (first intersect))) (Math/abs (- 0 (second intersect)))))
               smallest_distance
               (+ (Math/abs (- 0 (first intersect))) (Math/abs (- 0 (second intersect)))))))))

(defn -main
  "I call the functions for the Advent of Code"
  []
  (println "Day 1.1 - Module masses only:" (day1_1 day1_masses))
  (println "Day 1.2 - Fuel for the fuel:" (reduce + (map day1_2 day1_masses)))
  (println "Day 2.1 - Intcode output: " (day2_1 (day2_prep day2_intcode 12 2)))
  (println "Day 2.2 - Noun/verb:" (day2_2_result (day2_2_nounverb day2_intcode 99 19690720)))
  ;(println "Day 3.1 - Manhattan distance:" (day3_1_manhattan_distance (day3_1_all_intersections_for_both_lines day3_line1 day3_line2)))
  (println "Day 3.1 - Manhattan distance:" (day3_1_manhattan_distance day3_line1 day3_line2))
  )
