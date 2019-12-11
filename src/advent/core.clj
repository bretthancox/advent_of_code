(ns advent.core
  (:gen-class)
  (:require [clojure.string :as str]
            [advent.day1 :refer :all]
            [advent.day2 :refer :all]
            [advent.day3 :refer :all]
            [advent.inputs :refer :all]))


  

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
              :else (println "Day not completed yet"))
        (recur (rest days) (first (rest days))))))
        )
