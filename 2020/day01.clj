#!/usr/bin/env bb
(ns day01
  (:require
    [clojure.string :as str]))

(def *input* (set (->> *in* slurp str/split-lines (map #(Integer/parseInt %)))))

(defn find-pair [input target-sum]
  (let [has-partner (fn [x] (input (- target-sum x)))
        first-number (some has-partner input)]
    (when first-number
      #{first-number (- target-sum first-number)})))

(defn solve-task1
  ([input] (solve-task1 input 2020))
  ([input target-sum]
   (let [[x y] (vec (find-pair input target-sum))]
     (* x y))))

(defn find-triplet [input number]
  (let [new-input (disj input number)
        new-target-sum (- 2020 number)]
    (when-let [pair (find-pair new-input new-target-sum)]
      (conj pair number))))

(defn solve-task2 [input]
  (let [triplet (some (partial find-triplet input) input)]
    (apply * triplet)))

(println "Answer for star 1: " (solve-task1 *input*))
(println "Answer for star 2: " (solve-task2 *input*))