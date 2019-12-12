(ns advent.core
  (:gen-class)
  (:require [clojure.string :as str]
            [advent.day1 :refer :all]
            [advent.day2 :refer :all]
            [advent.day3 :refer :all]
            [advent.day4 :refer :all]
            [advent.inputs :refer :all]))


;; ----------- Day 5.1 -----------

;; Intcode changes for Day 5:
  ;; 1. The first code can now be up to 5 digits long instead of being 1, 2, or 99
    ;; a. The code is now divided into ADBCDE:
      ;; i. DE is the (now two digit) opcode (where D is only guaranteed to be present if the following parameter modes are specified)
      ;; ii. C is the mode to be applied to the first parameter
      ;; iii. B is the mode to be applied to the second parameter
      ;; iv. A is the mode to be applied third parameter
      ;; v. If A, B, or C are absent, assume they are 0
      ;; vi. If a mode is 0, the code behaves as per Day 2. If 1 is present, the associated parameter is not a pointer; it is the actual value
    ;; b. Opcode can now be:
      ;; i. 99 == halt
      ;; ii. 01 == sum 
      ;; iii. 02 == multiply 
      ;; iv. 3 == take a single integer input as its only parameter, so E is the instruction, D(C, B, A) is the integer to save it to first parameter location
      ;; v. 4 == output the value at first parameter location
  ;; 2. Intcode can now be 4 indices (for opcode 01, 02), 1 index (99), or 2 indices (3, 4), so the opcode also defines the step to take for the next intcode set
  ;; 3. An input function is needed that turns human-entered values into integers

(defn user_input_to_int
  "I turn a human-entered value into an integer"
  []
  (Integer/parseInt (read-line)))


(defn opcode_reverse_and_dissect
  "I take the new, longer opcode and reverse it, then dissect it into the meaningful components"
  [opcode]
  (let [opcode_details {:opcode nil :par1_mode 0 :par2_mode 0 :par3_mode 0}
        opcode_length (count (str opcode))
        opcode (apply str (reverse (str opcode)))]
    (if (= opcode "99")
      {:opcode 99}
      (case opcode_length
        1 (assoc opcode_details 
                 :opcode (Integer/parseInt (str (nth opcode 0))))
        2 (assoc opcode_details 
                 :opcode (Integer/parseInt (str (nth opcode 0))))
        3 (assoc opcode_details 
                 :opcode (Integer/parseInt (str (nth opcode 0)))
                 :par1_mode (Integer/parseInt (str (nth opcode 2))))
        4 (assoc opcode_details 
                 :opcode (Integer/parseInt (str (nth opcode 0)))
                 :par1_mode (Integer/parseInt (str (nth opcode 2)))
                 :par2_mode (Integer/parseInt (str (nth opcode 3))))
        5 (assoc opcode_details 
                 :opcode (Integer/parseInt (str (nth opcode 0)))
                 :par1_mode (Integer/parseInt (str (nth opcode 2)))
                 :par2_mode (Integer/parseInt (str (nth opcode 3)))
                 :par3_mode (Integer/parseInt (str (nth opcode 4))))))))


(defn revised_one_and_two_behavior
  [intcode posb posc posd opcode_details operator]
  (let [par1 (if (= (:par1_mode opcode_details) 0) 
               (get intcode (get intcode posb))
               (get intcode posb))
        par2 (if (= (:par2_mode opcode_details) 0)
               (get intcode (get intcode posc))
               (get intcode posc))
        par3 (if (= (:par3_mode opcode_details) 0)
               (get intcode posd)
               posd)]
    (assoc intcode par3 
           (day2_operator 
            par1 
            par2
            operator))))


(defn day5_1_opcode
  "I check the opcode and perform the appropriate replacements of items based on the primary rules. Opcode = index 0; Noun = index 1; Verb = index 2; Insert_at = index 3"
  [intcode]
  (loop [posa 0
         posb 1
         posc 2
         posd 3
         intcode intcode]
    (let [opcode (get intcode posa)
          opcode_details (opcode_reverse_and_dissect opcode)]
      (cond
        (= (:opcode opcode_details) 99) intcode
        (= (:opcode opcode_details) 1) (recur (+ posa 4) (+ posb 4) (+ posc 4) (+ posd 4)
                                              (revised_one_and_two_behavior intcode posb posc posd opcode_details +))
        (= (:opcode opcode_details) 2) (recur (+ posa 4) (+ posb 4) (+ posc 4) (+ posd 4)
                                              (revised_one_and_two_behavior intcode posb posc posd opcode_details *))
        (= (:opcode opcode_details) 3) (recur (+ posa 2) (+ posb 2) (+ posc 2) (+ posd 2)
                                              (assoc intcode 
                                                     (if (= (:par1_mode opcode_details) 0)
                                                       (get intcode posb)
                                                       posb)
                                                     (Integer/parseInt (read-line))))
        (= (:opcode opcode_details) 4) (do
                                         (println "Diagnostic code:" (if (= (:par1_mode opcode_details) 0)
                                                    (get intcode (get intcode posb))
                                                    (get intcode posb)))
                                         ;(println intcode)
                                         (recur (+ posa 2) (+ posb 2) (+ posc 2) (+ posd 2) intcode))
        :else (recur (+ posa 4) (+ posb 4) (+ posc 4) (+ posd 4) intcode)))))


;; ----------- Day 5.2 -----------


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
                            (println "Day 4.1 - Number of combinations:" (day4_1_command_and_control day4_start day4_end))
                            (println "Day 4.2 - No more than two identical neighboring values:" (day4_2_command_and_control day4_start day4_end)))
              (= day "5") (do
                            (println "Day 5.1 - A/C Intcode:") (day5_1_opcode day5_intcode)
                            (println "Day 5.2 - Not done"))
              :else (println "Day not completed yet"))
        (recur (rest days) (first (rest days))))))
        )
