#!/usr/bin/env bb
(ns day04
  (:require [clojure.string :as str]))
  
(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
       (catch AssertionError e
          (ex-message e)))))
            
(def input-path (or (first *command-line-args*) "2022/inputs/day04.txt"))

(defn parse-assignment [assignment-str]
  (let [[l r] (str/split assignment-str #"-")]
    [(parse-long l) (parse-long r)]))

(defn parse-line [line]
  (let [[l r] (str/split line #",")]
    [(parse-assignment l) (parse-assignment r)]))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map parse-line)))

(def input (parse-input (slurp input-path)))

(def test-input (parse-input "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"))

;;; Task 1

(defn left-contains-right?
  {:test #(do
            (assert (left-contains-right? [1 4] [2 3]))
            (assert (left-contains-right? [1 4] [1 4]))
            (assert (not (left-contains-right? [1 4] [3 5]))))}
  [[left-low left-high] [right-low right-high]]
  (and (<= left-low right-low) (<= right-high left-high)))

(defn one-contains-other? [left right]
  (or
    (left-contains-right? left right)
    (left-contains-right? right left)))

(defn task1 [input]
  (->> input
    (filter (fn [[left right]] (one-contains-other? left right)))
    count))
  
;;; Task 2  

(defn overlaps?
  {:test #(do
            (assert (overlaps? [1 4] [2 3]))
            (assert (overlaps? [3 5] [1 4]))
            (assert (not (overlaps? [2 4] [6 8])))
            (assert (not (overlaps? [14 91] [1 10]))))}
  [left right]
  (let [[left-low left-high] left
        [right-low right-high] right]
    (or
      (and (<= right-low left-high) (<= left-low right-high))
      (and (<= left-low right-high) (<= right-low left-high))
      (one-contains-other? left right))))


(defn task2
  {:test #(assert (= 4 (task2 test-input)))}
  [input]
  (count (filter (fn [[left right]] (overlaps? left right)) input)))


;;; Print output
;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))