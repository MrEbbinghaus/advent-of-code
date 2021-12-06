#!/usr/bin/env bb
(ns day06)

(def input-path (or (first *command-line-args*) "2021/inputs/day06.txt"))

(defn parse-input [input]
  (map parse-long (re-seq #"\d" input)))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "3,4,3,1,2"))

(defn- tick-group [[timer amount]]
  (if (zero? timer)
    {6 amount
     8 amount}
    {(dec timer) amount}))

(defn tick
  ([fishes] (apply merge-with + (map tick-group fishes)))
  ([gen fishes]
   (if (zero? gen)
     fishes
     (recur (dec gen) (tick fishes)))))


(defn fishes-after
  {:test #(do (assert (= 5934 (fishes-after 80 test-input)))
              (assert (= 26984457539 (fishes-after 256 test-input))))}
  [gen input]
  (reduce + (vals (tick gen (frequencies input)))))

;;; Task 1
(def task1 (partial fishes-after 80))
;;; Task 2
(def task2 (partial fishes-after 256))


;;; Print output
(print "Task 1: ")
(println (task1 input))
(print "Task 2: ")
(println (task2 input))