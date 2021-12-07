#!/usr/bin/env bb
(ns day07)

(def input-path (or (first *command-line-args*) "2021/inputs/day07.txt"))

(defn parse-input [input]
  (map parse-long (re-seq #"\d+" input)))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "16,1,2,0,4,2,7,1,2,14"))

(defn linear-distance [target crab]
  (Math/abs ^long (- target crab)))

(defn gauss-distance [target crab]
  (let [x (linear-distance target crab)]
    (/ (+ (* x x) x) 2)))

(defn median [crabs]
  (let [x (quot (count crabs) 2)]
    (nth (sort crabs) x)))

(defn mean [crabs]
  (/ (reduce + crabs) (count crabs)))

(defn cumulative-fuel [distance-fn target crabs]
  (reduce + (map #(distance-fn target %) crabs)))

;;; Task 1


(defn task1
  {:test #(assert (= 37 (task1 test-input)))}
  [input]
  (let [median (median input)]
    (min
      (cumulative-fuel linear-distance (median input) input)
      (cumulative-fuel linear-distance (median input) input))))


;;; Task 2

;; Details: https://twitter.com/MrEbbinghaus/status/1468359022453612549
(defn task2
  {:test #(assert (= 168 (task2 test-input)))}
  [input]
  (let [candidates ((juxt #(Math/floor %) #(Math/ceil %)) (mean input))]
    (int (apply min (map #(cumulative-fuel gauss-distance % input) candidates)))))

;;; Print output
(print "Task 1: ")
(println (task1 input))
(print "Task 2: ")
(println (task2 input))