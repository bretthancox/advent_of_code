(ns advent.day4-test
  (:require [clojure.test :refer :all]
            [advent.day4 :refer :all]))

            
(deftest day4_order_equal_inc
  (testing "Confirming that the check for equal or incrementing order returns the correct result"
    (is (true? (check_equal_incrementing_order [1 2 2 4 5])))
    (is (false? (check_equal_incrementing_order [1 2 2 1 5])))))

(deftest day4_check_one_pair_equal
  (testing "Confirming that checking for one equal pair produces the correct result"
    (is (true? (check_at_least_one_equal_pair [1 2 2 4 5])))
    (is (false? (check_at_least_one_equal_pair [1 2 3 4 5])))))

(deftest day4_check_int_to_string
  (testing "Does the int get appropriately converted"
    (is (= (convert_int_to_string 12245) "12245"))
    (is (= (convert_int_to_string 12345) "12345"))))

(deftest day4_split_string
  (testing "Does the string get split correctly"
    (is (= (make_vec_of_strings "12245") ["1" "2" "2" "4" "5"]))))

(deftest day4_produce_ints
  (testing "Do the substrings get converted to integers"
    (is (= (make_substrings_into_ints ["1" "2" "2" "4" "5"]) [1 2 2 4 5]))))

(deftest day4_ensure_both_checks_are_true
  (testing "Confirm that when both checks are true, the result is true"
    (is (true? (check_both_checks_are_true [1 2 2 4 5])))
    (is (false? (check_both_checks_are_true [1 2 3 4 5])))
    (is (false? (check_both_checks_are_true [1 2 2 3 2])))))

(deftest day4_end_to_end_int_to_vecint
  (testing "Confirm that the end to end process of building the vec of ints works"
    (is (= (produce_vector_of_ints 12245) [1 2 2 4 5]))))

(deftest day4_1_test
  (testing "Confirm the results are correct"
    (is (= (day4_1_command_and_control 122342 122348) 5))
    (is (= (day4_1_command_and_control 112224 112233) 7))))


(deftest day4_2_logic_check
  (testing "Checking the logic of picking a password with only two neighboring values"
    (is (true? (check_if_more_than_two_identical_values_neighboring [1 2 2 4 5])))
    (is (false? (check_if_more_than_two_identical_values_neighboring [1 2 2 2 5])))))

(deftest day4_2_test
  (testing "Confirm the results are correct"
    (is (= (day4_2_command_and_control 122342 122348) 5))
    (is (= (day4_2_command_and_control 112224 112233) 7))))