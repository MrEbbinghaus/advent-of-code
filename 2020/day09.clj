#!/usr/bin/env bb
(ns day09
  (:require [clojure.string :as str]))

(def *input* (time (->> *in* slurp str/split-lines (map #(Long/parseLong %)))))

(defn has-pair?
  [input sum]
  (letfn [(has-partner [x] (get input (- sum x)))]
    (some has-partner input)))

(defn solve-task1 [input]
  (some
    (fn not-valid [x]
      (let [preamble (set (butlast x))
            sum (last x)]
        (when (not (has-pair? preamble sum))
          sum)))
    (partition 26 1 input)))

(defn solve-task2 [input]
  (let [input (vec input)
        target (solve-task1 input)]
    (loop [start 0 end 2]
      (let [r (subvec input start end)
            sum (apply + r)]
        (cond
          (= sum target)
          (+ (apply min r) (apply max r))

          (> sum target)
          (recur (inc start) end)                           ; shrink window from the back

          :else
          (recur start (inc end)))))))                      ; expand window on the front

(println "Answer for star 1: " (time (solve-task1 *input*)))
(println "Answer for star 2: " (time (solve-task2 *input*)))