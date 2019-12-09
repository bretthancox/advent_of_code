(ns advent.day2
  (:gen-class))

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
  "I check the opcode and perform the appropriate replacements of items based on the primary rules. Opcode = index 0; Noun = index 1; Verb = index 2; Insert_at = index 3"
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
