(ns advent.core
  (:gen-class)
  (:require [advent.inputs :refer [masses]]))

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

(defn -main
  "I call the functions for the Advent of Code"
  []
  (println "Day 1.1 - Module masses only:" (day1_1 masses))
  (println "Day 1.2 - Fuel for the fuel:" (reduce + (map day1_2 masses))))
