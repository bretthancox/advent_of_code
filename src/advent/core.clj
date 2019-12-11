(ns advent.core
  (:gen-class)
  (:require [clojure.string :as str]
            [advent.day1 :refer :all]
            [advent.day2 :refer :all]
            [advent.day3 :refer :all]
            [advent.inputs :refer :all]))


;; ----------- Day 4.1 -----------
  
(defn check_equal_incrementing_order
  "I check if the numbers are equal or increase across the number being tested"
  [vector_of_ints]
  (loop [vector_of_ints vector_of_ints
         final_state true]
    (if (or (= 1 (count vector_of_ints)) (false? final_state))
      final_state
      (recur 
       (rest vector_of_ints) 
       (if 
        (> (first vector_of_ints) (second vector_of_ints))
         false
         true)))))


(defn check_at_least_one_equal_pair
  "I check if at least one neighboring pair of numbers are equal"
  [vector_of_ints]
  (loop [vector_of_ints vector_of_ints
         final_state false]
    (if (or (= 1 (count vector_of_ints)) (true? final_state))
      final_state
      (recur 
       (rest vector_of_ints) 
       (if 
        (= (first vector_of_ints) (second vector_of_ints))
         true
         false)))))

(defn convert_int_to_string
  "I convert an integer to a string"
  [integer]
  (str integer))


(defn make_vec_of_strings
  "I convert a string into a vector of individual characters"
  [string]
  (str/split string #""))


(defn make_substrings_into_ints
  "I convert the substrings into integers"
  [vec_of_strings]
  (into [] (map #(Integer/parseInt %) vec_of_strings)))


(defn check_both_checks_are_true
  "I perform the two checks and confirm that both return true"
  [vector_of_ints]
  (if (and (check_at_least_one_equal_pair vector_of_ints) (check_equal_incrementing_order vector_of_ints))
    true
    false))

(defn produce_vector_of_ints
  [number]
  (->> number
       (convert_int_to_string)
       (make_vec_of_strings)
       (make_substrings_into_ints)))


(defn day4_2_command_and_control
  [start end]
  (loop [start start
         end end
         counter 0]
    (if (> start end)
      counter
      (if (true? (check_both_checks_are_true (produce_vector_of_ints start)))
        (recur 
         (inc start)
         end
         (inc counter))
        (recur
         (inc start)
         end
         counter)))))


(defn -main
  "I call the functions for the Advent of Code on the basis of which day(s) are added as arguments to the command line call"
  [& days]
  (loop [days days
         day (first days)]
    (if (empty? days)
      nil
      (do
        (cond (= day "1") (do
                            (println "Day 1.1 - Module masses only:" (day1_1 day1_masses))
                            (println "Day 1.2 - Fuel for the fuel:" (reduce + (map day1_2 day1_masses))))
              (= day "2") (do 
                            (println "Day 2.1 - Intcode output: " (day2_1 (day2_prep day2_intcode 12 2)))
                            (println "Day 2.2 - Noun/verb:" (day2_2_result (day2_2_nounverb day2_intcode 99 19690720))))
              (= day "3") (do 
                            (println "Day 3.1 - Manhattan distance:" (day3_1_manhattan_distance day3_line1 day3_line2))
                            (println "Day 3.2 - Shortest route to intersect:" (day3_2_loop_intersects day3_line1 day3_line2)))
              (= day "4") (do
                            (println "Day 4.1 - Number of combinations:" (day4_2_command_and_control day4_start day4_end)))
              :else (println "Day not completed yet"))
        (recur (rest days) (first (rest days))))))
        )
