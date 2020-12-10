#!/usr/bin/env bb
(ns day10
  (:require [clojure.string :as str]))

(def *input (->> *in* slurp str/split-lines (map #(Integer/parseInt %)) sort))

(defn solve-task1 [input]
  (let [jolts
        (->>
          (conj input 0)                                    ; outlet
          (partition 2 1)                                   ; pair up a connection
          (map (fn [[x y]] (- y x)))                        ; get difference of connection pair
          frequencies)]
    (* (jolts 1)
      (inc (jolts 3)))))                                    ; there is always a 3 jolt connection to  device

(defn build-graph
  "Builds a map of outlet -> #{fitting next outlets}"
  [input]
  (->>
    (conj input 0)                                          ; outlet
    (partition-all 4 1)
    (reduce
      (fn [m [outlet & jumps]]
        (assoc m outlet (filter #(>= 3 (- % outlet)) jumps)))
      {})))

(def number-of-possible-ways
  (memoize
    (fn [graph start]
      (if (empty? (get graph start))
        1                                                   ; connection to device
        (reduce + (map (partial number-of-possible-ways graph) (get graph start)))))))

(defn solve-task2 [input]
  (number-of-possible-ways (build-graph input) 0))


(println "Answer for star 1: " (time (solve-task1 *input)))
(println "Answer for star 2: " (time (solve-task2 *input)))

