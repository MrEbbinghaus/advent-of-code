#!/usr/bin/env bb
(ns day06
  (:require [clojure.string :as str]))
  
(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
       (catch AssertionError e
          (ex-message e)))))
            
(def input-path (or (first *command-line-args*) "2022/inputs/day06.txt"))

(defn parse-input [input]
  ;; TODO Implement me!
  input)

(def input (parse-input (slurp input-path)))


;;; Task 1
(defn all-different?
  {:test
   #(do
      (assert (all-different? [1 2 3 4]))
      (assert (not (all-different? [1 4 3 4]))))}
  [frame]
  (= (count frame) (count (set frame))))

(defn task1
  {:test
   #(do
      (assert (= 5 (task1 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
      (assert (= 6 (task1 "nppdvjthqldpwncqszvftbrmjlhg")))
      (assert (= 10 (task1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
      (assert (= 11 (task1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))))}
  [input]
  (+ 4 (count (take-while (complement all-different?) (partition 4 1 input)))))
;;; Task 2  


(defn task2
  {:test
   #(do
      (assert (= 19 (task2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
      (assert (= 23 (task2 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
      (assert (= 23 (task2 "nppdvjthqldpwncqszvftbrmjlhg")))
      (assert (= 26 (task2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))))}
  [input]
  (+ 14 (count (take-while (complement all-different?) (partition 14 1 input)))))

(all-tests)

;;; Print output
;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))