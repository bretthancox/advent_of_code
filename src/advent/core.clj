(ns advent.core
  (:gen-class)
  (:require [advent.inputs :refer [day1_masses day2_intcode]]))

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
  [int1 int2]
  ;(println int1 int2)
  (+ int1 int2))

(defn day2_multiply
  [int1 int2]
  (* int1 int2))

(defn day2_prep
  [no_alarm]
  (assoc (assoc no_alarm 1 12) 2 2))

(defn day2_1
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
           (assoc intcode (get intcode posd) (day2_add (get intcode (get intcode posb)) (get intcode (get intcode posc))))
           ;(get intcode (+ posa 4))
           )
          (if (= opcode 2)
            (recur
             (+ posa 4)
             (+ posb 4)
             (+ posc 4)
             (+ posd 4)
             (assoc intcode (get intcode posd) (day2_multiply (get intcode (get intcode posb)) (get intcode (get intcode posc))))
             ;(get intcode (+ posa 4))
             )
            (recur (+ posa 4) (+ posb 4) (+ posc 4) (+ posd 4) intcode ;(get intcode (+ posa 4))
                   )))))))

(defn -main
  "I call the functions for the Advent of Code"
  []
  (println "Day 1.1 - Module masses only:" (day1_1 day1_masses))
  (println "Day 1.2 - Fuel for the fuel:" (reduce + (map day1_2 day1_masses)))
  (println "Day 2.1 - Intcode output: "(day2_1 (day2_prep day2_intcode))))
