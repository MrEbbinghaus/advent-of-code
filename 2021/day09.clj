#!/usr/bin/env bb
(ns day09
  (:require [clojure.string :as str]))

(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
          (catch AssertionError e
            (ex-message e)))))

(defn parse-digit [b]
  (- (byte b) (byte \0)))

(def input-path (or (first *command-line-args*) "2021/inputs/day09.txt"))

(defn parse-input [input]
  (into-array
    (into []
      (comp
        (map parse-digit)
        (partition-by #{-38})                                 ; \n - \0
        (take-nth 2)
        (map byte-array))
      input)))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"))

(defn print-map [input]
  (doseq [line input]
    (println (vec line))))

;;; Task 1

(defn points-around [[x y]]
  [           [x (dec y)]
   [(dec x) y]           [(inc x) y]
              [x (inc y)]])

(defn low-point? [input coords]
  (let [p (get-in input coords)]
    (every? #(< p (get-in input % ##Inf)) (points-around coords))))

(defn low-points [input]
  (for [width (range (count input))
        height (range (count (first input)))
        :when (low-point? input [width height])]
    [width height]))

(defn task1
  {:test #(assert (= 15 (task1 test-input)))}
  [input]
  (->> input
    low-points
    (map #(inc (get-in input %)))
    (reduce +)))

(defn direction [p1 p2]
  (case (mapv - p1 p2)
    [0 1] \←
    [0 -1] \→
    [1 0] \↑
    [-1 0] \↓
    [0 0] \x))

(defn draw-basins [input]
  (for [width (range (count input))]
    (for [height (range (count (first input)))
          :let [pos [width height]]]
      (if (= 9 (get-in input pos))
       \space
       (->> pos
         (conj (points-around pos))
         (sort-by #(get-in input % ##Inf))
         first
         (direction pos))))))

;;; Task 2
(defn basin [input point]
  (let [val (get-in input point ##Inf)]
    (cond
      (< 8 val) nil
      (< 7 val) #{point}
      :else (conj (->> point
                    points-around
                    (filter #(< val (get-in input % ##Inf)))
                    (mapcat #(basin input %))
                    set)
              point))))

(defn task2
  {:test #(assert (= 1134 (task2 test-input)))}
  [input]
  (let [basin #(basin input %)]
    (->> input
      low-points
      (map (comp count basin))
      (sort >)
      (take 3)
      (reduce *))))

;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (time (task2 input)))
