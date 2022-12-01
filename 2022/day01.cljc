#!/usr/bin/env bb
(ns day01
  (:require [clojure.string :as str]))
  
(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
       (catch AssertionError e
          (ex-message e)))))

(def input-path (or (first *command-line-args*) "2022/inputs/day01.txt"))

(defn parse-input [input]
  (->> input
    str/split-lines
    (partition-by #{""})
    (remove #{[""]})
    (map #(map parse-long %))))

(def input (parse-input (slurp input-path)))

;;; Task 1

(defn sum-snacks [elf]
  (apply + elf))

(defn task1 [elfs]
  (apply max (map sum-snacks elfs)))
  
;;; Task 2  
  
  
(defn task2 [elfs]
  (->> elfs
    (map sum-snacks)
    (sort)
    (take-last 3)
    (reduce +)))


;;; Print output
;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))