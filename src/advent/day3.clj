(ns advent.day3
  (:gen-class)
  (:require [clojure.string :as str]))

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
      (if (empty? horiz_loop) ;; ...we can reuse the original vector as passed to the function even if we empty out the copy
        (recur (rest vert_loop) horiz_lines intersections) 
        (let [vert_coords (first vert_loop)
              horiz_coords (first horiz_loop)
              vert_x_axis (nth vert_coords 2)
              horiz_y_axis (nth horiz_coords 2)
              ;; A little cheat. The end of a line can have a smaller number than the start
              ;; and catching this situation with code is painful. So I guarantee that the smallest
              ;; x or y value is x1 or y1 and the largest is x2 or y2, simplifying subsequent math.
              ;; Since the direction of the line is irrelevant to determining intersects, this is not a problem.
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


;; ----------- Day 3.2 -----------

(defn crossproduct
  "I determine if an intersection point is on the line defined by point1 and point2"
  [point1 point2 testpoint]
  (let [part1 (- (second testpoint) (second point1))
        part2 (- (first point2) (first point1))
        part3 (- (first testpoint) (first point1))
        part4 (- (second point2) (second point1))
        crossprd (- (* part1 part2) (* part3 part4))]
    (if (= crossprd 0)
      true
      false)))

(defn day3_2_crossproduct_output
  "I look at each sub-line in the line coordinates and determine if the point of intersect
is on that line. If it isn't add the coordinates to the coordinates_to_intersect. If it is,
add the coordinate of the point of intersection to the coordinates_to_intersect as the last
item and return the coordinates_to_intersect."
  [intersection line_coordinates]
  (loop [line_coordinates line_coordinates
         line_start (first line_coordinates)
         line_end (second line_coordinates)
         coordinates_to_intersect [line_start]
         is_intersected (crossproduct line_start line_end intersection)]
    ;(println (conj coordinates_to_intersect intersection))
    (if (true? is_intersected)
      (conj coordinates_to_intersect intersection)
      (recur 
       (rest line_coordinates) 
       line_end
       (second (rest line_coordinates)) 
       (conj coordinates_to_intersect line_end)
       (crossproduct line_end (second (rest line_coordinates)) intersection)))))

(defn sum_the_distance
  "For a given set of coordinates, sum the distance travelled and return."
  [coords_to_intersect]
  (loop [coords_to_intersect coords_to_intersect
         point1 (first coords_to_intersect)
         point2 (second coords_to_intersect)
         total 0]
    (if (= (count coords_to_intersect) 1)
      total
      (recur 
       (rest coords_to_intersect)
       point2
       (second (rest coords_to_intersect))
       (+ total (reduce + (map #(Math/abs %) (map - point2 point1))))))))

(defn day3_2_loop_intersects
  "For each intersect, determine the coordinates to that point for each line, sum
the distance, add to a vector, and then return the smallest value in the vector."
  [line1 line2]
  (let [line1_coords (day3_1_coordinate_builder line1)
        line2_coords (day3_1_coordinate_builder line2)
        intersections (day3_1_all_intersections_for_both_lines line1 line2)
        distances []]
    (loop [intersections intersections
           line1_intersection_coords (day3_2_crossproduct_output (first intersections) line1_coords)
           line2_intersection_coords (day3_2_crossproduct_output (first intersections) line2_coords)
           distances distances]
      ;(println (count intersections) (count distances))
      (if (= 1 (count intersections))
        (first (sort distances))
        (recur 
         (rest intersections)
         (day3_2_crossproduct_output (first (rest intersections)) line1_coords)
         (day3_2_crossproduct_output (first (rest intersections)) line2_coords)
         (conj distances (+ (sum_the_distance line1_intersection_coords) (sum_the_distance line2_intersection_coords))))))))