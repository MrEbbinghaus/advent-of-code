#!/usr/bin/env bb
(ns day03
  (:require
    [clojure.string :as str]))

(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
          (catch AssertionError e
            (ex-message e)))))

(def input-path (or (first *command-line-args*) "2021/inputs/day03.txt"))

(defn parse-input [input]
  (->> input
    str/split-lines
    (map #(mapv parse-long (re-seq #"[01]" %)))))

(def input (parse-input (slurp input-path)))

(def test-input
  (parse-input "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"))

(defn bits->int [bits]
  (Integer/parseInt (apply str bits) 2))

(defn transpose [bits-seq]
  (apply map vector bits-seq))

(def flip-bit {0 1, 1 0})

(defn flip [bits]
  (map flip-bit bits))

(defn most-common
  {:test #(assert (= 1 (most-common (map first test-input))))}
  [bits]
  (let [f (frequencies bits)]
    (if (<= (get f 0) (get f 1))
      1 0)))

(def least-common (comp flip-bit most-common))

(defn fold-most-common [input]
  (map most-common (transpose input)))

(defn gamma-rate
  {:test #(assert (= 22 (gamma-rate test-input)))}
  [input]
  (->> input fold-most-common bits->int))

(defn epsilon-rate
  {:test #(assert (= 9 (epsilon-rate test-input)))}
  [input]
  (->> input fold-most-common flip bits->int))

(defn task1 [input]
  (* (gamma-rate input) (epsilon-rate input)))

;;; TASK 2

(defn select-by-bit-criteria [input criteria-fn]
  (loop [bit-pos 0
         numbers input]
    (if (<= (count numbers) 1)
      (first numbers)
      (let [criteria-bit (criteria-fn (map #(nth % bit-pos) numbers))]
        (recur
          (inc bit-pos)
          (filter
            #(= criteria-bit (nth % bit-pos))
            numbers))))))

(defn oxygen-rating
  {:test #(assert (= 23 (oxygen-rating test-input)))}
  [input]
  (bits->int (select-by-bit-criteria input most-common)))

(defn co2-rating
  {:test #(assert (= 10 (co2-rating test-input)))}
  [input]
  (bits->int (select-by-bit-criteria input least-common)))

(defn task2 [input]
  (* (oxygen-rating input) (co2-rating input)))


;;; Print output
(print "Task 1: ")
(println (task1 input))
(print "Task 2: ")
(println (task2 input))