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
    (println number_of_items)
    (loop [noun 0 verb 0]
      (println "Noun:" noun "Verb:" verb)
      (let [result (day2_1 (day2_prep intcode noun verb))]
        (println result 0)
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


(defn -main
  "I call the functions for the Advent of Code"
  []
  (println "Day 1.1 - Module masses only:" (day1_1 day1_masses))
  (println "Day 1.2 - Fuel for the fuel:" (reduce + (map day1_2 day1_masses)))
  (println "Day 2.1 - Intcode output: " (day2_1 (day2_prep day2_intcode 12 2)))
  (println "Day 2.2 - Noun/verb:" (day2_2_result (day2_2_nounverb day2_intcode)))
  )
