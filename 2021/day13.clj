#!/usr/bin/env bb
(ns day13
  (:require [clojure.string :as str]))
            
(def input-path (or (first *command-line-args*) "2021/inputs/day13.txt"))

(defn parse-input [input]
  (let [[points folds] (str/split input #"\n\n")]
    {:paper (->> (str/split points #"[\n,]")
              (map parse-long)
              (partition 2)
              set)
     :folds (->> folds
              (re-seq #"([xy])=(\d+)")
              (map #(vector (second %) (parse-long (last %)))))}))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"))

(defn fold-at [x fold-line]
  (- x (* 2 (- x fold-line))))

(defn fold-point [[orientation fold-line] [x y]]
  (if (= "x" orientation)
    [(if (< fold-line x)
       (fold-at x fold-line)
       x)
     y]
    [x
     (if (< fold-line y)
       (fold-at y fold-line)
       y)]))

(defn fold [paper fold-cmd]
  (set
    (map
      #(fold-point fold-cmd %)
      paper)))


;;; Task 1
(defn task1
  {:test #(assert (= 17 (task1 test-input)))}
  [input]
  (count (fold (:paper input) (first (:folds input)))))

;;; Task 2
(defn print-paper [paper]
  (doseq [y (range (inc (apply max (map second paper))))]
    (doseq [x (range (inc (apply max (map first paper))))]
      (print (if (contains? paper [x y])
               "#"
               ".")))
    (println)))

(defn task2 [input]
  (print-paper
    (loop [paper (:paper input)
           [fold-cmd & r] (:folds input)]
      (if fold-cmd
        (recur (fold paper fold-cmd) r)
        paper))))

;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))