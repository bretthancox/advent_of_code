(ns advent.core
  (:gen-class)
  (:require [clojure.string :as str]
            [advent.inputs :refer [day1_masses day2_intcode]]))

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

(defn day2_add
  "Extracts repeated additions from other code"
  [int1 int2]
  (+ int1 int2))

(defn day2_multiply
  "Extracts repeated multiplications from other code"
  [int1 int2]
  (* int1 int2))

(defn day2_prep
  "Performs the preparation task of replacing the 'noun' and 'verb' per the 2.2 description (aka index 1 and index 2) with new values"
  [no_alarm noun verb]
  (assoc (assoc no_alarm 1 noun) 2 verb))

(defn day2_1
  "Checks the opcode and performs the appropriate replacements of items based on the primary rules. Opcode = index 0; Noun = index 1; Verb = index 2; Insert_at = index 3"
  [intcode]
  (loop [posa 0
         posb 1 
         posc 2 
         posd 3         
         intcode intcode]
    (let [opcode (get intcode posa)]
      (if (= opcode 99)
        intcode
        (if (= opcode 1)
          (recur
           (+ posa 4)
           (+ posb 4)
           (+ posc 4)
           (+ posd 4)
           ;; This horrible line does the following: 
             ;; for index 1 and 2, get the value at index 1 and 2, then use those values as indices to get the values for calculation
             ;; then, get the target_index value at index 3, take the calculation result, and write the calculation result to the target_index
           (assoc intcode (get intcode posd) (day2_add (get intcode (get intcode posb)) (get intcode (get intcode posc)))) 
           )
          (if (= opcode 2)
            (recur
             (+ posa 4)
             (+ posb 4)
             (+ posc 4)
             (+ posd 4)
             (assoc intcode (get intcode posd) (day2_multiply (get intcode (get intcode posb)) (get intcode (get intcode posc))))
             )
            (recur (+ posa 4) (+ posb 4) (+ posc 4) (+ posd 4) intcode
                   )))))))

;; ----------- Day 2.2 -----------


(defn day2_2_nounverb
  "I find the noun and verb that produce the desired outcome of 19690720 at index 0 and return them as a vector"
  [intcode]
  (let [number_of_items (count intcode)]
    ;(println number_of_items)
    (loop [noun 0 verb 0]
      ;(println "Noun:" noun "Verb:" verb)
      (let [result (day2_1 (day2_prep intcode noun verb))]
        ;(println result 0)
        (if (= (get result 0) 19690720)
          [(get result 1) (get result 2)]
          (recur 
           (if (= verb (- number_of_items 1)) (inc noun) noun)
           (if (= verb (- number_of_items 1)) 0 (inc verb)))
          )))))


(defn day2_2_result
  "I use the noun and verb vector to perform the calculation that produces the final answer for day 2.2"
  [nounverb]
  (+ (* 100 (get nounverb 0)) (get nounverb 1)))



;; ----------- Day 3.1 -----------

(defn day3_1_coordinate_vec_builder_x
  [operator coordinates next_str]
  (vector (operator (get coordinates 0)
             (Integer/parseInt (subs next_str 1)))
          (get coordinates 1)))

(defn day3_1_coordinate_vec_builder_y
  [operator coordinates next_str]
  (vector (get coordinates 0) ;; X
          (operator (get coordinates 1)  ;; Y
             (Integer/parseInt (subs next_str 1)))))

(defn day3_1_coordinate_builder
  [strings]
  (loop [next_str (first strings)
         rest_strings (rest strings)
         coordinates [0 0] 
         vec_of_coords [coordinates]] 
    (if (= 
         (+ 1 (count strings)) 
         (count vec_of_coords))
      vec_of_coords
      (if (str/starts-with? next_str "U")
        (recur
         (first rest_strings) 
         (rest rest_strings)
         (day3_1_coordinate_vec_builder_y + coordinates next_str)
         (conj vec_of_coords (day3_1_coordinate_vec_builder_y + coordinates next_str)))
        (if (str/starts-with? next_str "D")
          (recur
           (first rest_strings)
           (rest rest_strings)
           (day3_1_coordinate_vec_builder_y - coordinates next_str)
           (conj vec_of_coords (day3_1_coordinate_vec_builder_y - coordinates next_str)))
          (if (str/starts-with? next_str "R")
            (recur
             (first rest_strings)
             (rest rest_strings)
             (day3_1_coordinate_vec_builder_x + coordinates next_str)
             (conj vec_of_coords (day3_1_coordinate_vec_builder_x + coordinates next_str)))
            (if (str/starts-with? next_str "L")
              (recur
               (first rest_strings)
               (rest rest_strings) 
               (day3_1_coordinate_vec_builder_x - coordinates next_str)
               (conj vec_of_coords (day3_1_coordinate_vec_builder_x - coordinates next_str))))))))))


(defn day3_1_get_horizontals
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
  [vert_lines horiz_lines]
  (loop [vert_loop vert_lines
         horiz_loop horiz_lines
         ;vert_index 0
         ;horiz_index 0
         intersections []]
    
    (if (empty? vert_loop)
      intersections
      (if (empty? horiz_loop)
        (recur (rest vert_loop) horiz_lines intersections);(inc vert_index) 0 intersections)
        (let [vert_coords (first vert_loop)
              horiz_coords (first horiz_loop)
              vert_x_axis (nth vert_coords 2)
              horiz_y_axis (nth horiz_coords 2)
              vert_y1 (if (> (nth vert_coords 0) (nth vert_coords 1)) (nth vert_coords 1) (nth vert_coords 0))
              vert_y2 (if (> (nth vert_coords 0) (nth vert_coords 1)) (nth vert_coords 0) (nth vert_coords 1))
              horiz_x1 (if (> (nth horiz_coords 0) (nth horiz_coords 1)) (nth horiz_coords 1) (nth horiz_coords 0))
              horiz_x2 (if (> (nth horiz_coords 0) (nth horiz_coords 1)) (nth horiz_coords 0) (nth horiz_coords 1))]
          ;(println "Vertical:" vert_coords "Vertical x axis:" vert_x_axis "Vertical y1:" vert_y1 "Vertical y2:" vert_y2 "Horizontal:" horiz_coords "Horizontal y axis:" horiz_y_axis "Horizontal x1:" horiz_x1 "Horizontal x2:" horiz_x2)
          (if 
           (and
            (and (> horiz_y_axis vert_y1) (< horiz_y_axis vert_y2))
            (and (> vert_x_axis horiz_x1) (< vert_x_axis horiz_x2)))
            (recur vert_loop (rest horiz_loop) (conj intersections [vert_x_axis horiz_y_axis]))
            (recur vert_loop (rest horiz_loop) intersections)))))))


(defn day3_1_all_intersections_for_both_lines
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
  [intersection_points]
  (loop [intersection_points intersection_points
         smallest_distance (reduce + (first intersection_points))]
    (if (empty? intersection_points)
      smallest_distance
      (recur (rest intersection_points) (if (< smallest_distance (reduce + (first intersection_points)))
                                               smallest_distance
                                               (reduce + (first intersection_points)))))))

(defn -main
  "I call the functions for the Advent of Code"
  []
  (println "Day 1.1 - Module masses only:" (day1_1 day1_masses))
  (println "Day 1.2 - Fuel for the fuel:" (reduce + (map day1_2 day1_masses)))
  (println "Day 2.1 - Intcode output: " (day2_1 (day2_prep day2_intcode 12 2)))
  (println "Day 2.2 - Noun/verb:" (day2_2_result (day2_2_nounverb day2_intcode)))
  ;(println (map #(if (str/starts-with? % "R") % nil) ["R8","U5","L5","D3"]))
  ;(println (map day3_1_coordinate_changes ["R8","U5","L5","D3"]))
  ;(println "Day 3.1 - Coordinate Builder" (day3_1_coordinate_builder ["R8","U5","L5","D3"]))
  ;(println "Day 3.1 - Find Horizontal Lines" (day3_1_get_horizontals (day3_1_coordinate_builder ["R8","U5","L5","D3"])))
  ;(println "Day 3.1 - Find Vertical Lines" (day3_1_get_verticals (day3_1_coordinate_builder ["R8","U5","L5","D3"])))
  ;(println "Day 3.1 - Find intersection points" (day3_1_find_intersection_points 
  ;                                               (day3_1_get_verticals (day3_1_coordinate_builder ["U7","R6","D4","L4"]))
  ;                                               (day3_1_get_horizontals (day3_1_coordinate_builder ["R8","U5","L5","D3"]))))
  ;(println "Day 3.1 - Find intersection points" (day3_1_find_intersection_points
  ;                                               (day3_1_get_horizontals (day3_1_coordinate_builder ["U7","R6","D4","L4"]))
  ;                                               (day3_1_get_verticals (day3_1_coordinate_builder ["R8","U5","L5","D3"]))))
  ;(println (into [] (map #(name %) '[U7,R6,D4,L4])))
  (println (day3_1_manhattan_distance (day3_1_all_intersections_for_both_lines ["R8","U5","L5","D3"] ["U7","R6","D4","L4"])))
  
  )
