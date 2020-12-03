#!/usr/bin/env bb
(ns day03
  (:require
    [clojure.string :as str]))

(def *input* (->> *in* slurp str/split-lines (map cycle)))

(defn multiples-of [n] (map (partial * n) (range)))

(defn get-slope [input [right down]]
  (map nth (take-nth down input) (multiples-of right)))

(defn no-of-trees [slope]
  (-> slope frequencies (get \#)))

(defn solve-task1 [input]
  (-> input (get-slope [3 1]) no-of-trees))

(defn solve-task2 [input]
  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
       (map (partial get-slope input))
       (map no-of-trees)
       (reduce *)))

(println "Answer for star 1:" (time (solve-task1 *input*)))
(println "Answer for star 2:" (time (solve-task2 *input*)))