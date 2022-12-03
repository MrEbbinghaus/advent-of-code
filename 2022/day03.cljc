#!/usr/bin/env bb
(ns day03
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))
  
(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
       (catch AssertionError e
          (ex-message e)))))
            
(def input-path (or (first *command-line-args*) "2022/inputs/day03.txt"))



(defn parse-input [input]
  (str/split-lines input))

(def input (parse-input (slurp input-path)))

(defn priority
  {:test #(do
            (assert (= 1 (priority \a)))
            (assert (= 26 (priority \z)))
            (assert (= 27 (priority \A)))
            (assert (= 52 (priority \Z))))}
  [item]
  (let [char-point (int item)]
    (if (< char-point (int \a))
      (- char-point 38)
      (- char-point 96))))

;;; Task 1
(defn overlap [sets]
  (first (apply set/intersection sets)))

(defn split-in-half [s]
  (let [half (/ (count s) 2)]
    (split-at half s)))

(defn task1 [input]
  (->> input
       (map split-in-half)
       (map #(mapv set %))
       (transduce (map (comp priority overlap)) + 0)))
  
;;; Task 2
(defn split-elf-groups [input]
  (->> input
       (partition-all 3 3)
       (map #(mapv set %))))

(defn task2 [input]
  (->> input
       split-elf-groups
       (transduce (map (comp priority overlap)) + 0)))

;;; Print output
;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))