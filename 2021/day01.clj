#!/usr/bin/env bb
(ns day01
  (:require [clojure.string :as str]))

(def input (slurp (first *command-line-args*)))

(defn parse-input [input]
  (map #(Integer/parseInt %) (str/split-lines input)))

(defn task1 [input]
  (->> input
    (partition 2 1)
    (filter #(apply < %))
    count))

(defn task2 [input]
  (->> input
    (partition 3 1)
    (map #(reduce + %))
    task1))


(print "Task 1: ")
(println (task1 (parse-input input)))
(print "Task 2: ")
(println (task2 (parse-input input)))