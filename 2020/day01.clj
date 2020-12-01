#!/usr/bin/env bb
(ns day01
  (:require
    [clojure.string :as str]))

;; Read from stdin and return a set of integers
(def *input* (apply sorted-set (->> *in* slurp str/split-lines (map #(Integer/parseInt %)))))

(defn find-pair
  "In a set of `input` returns a set of two numbers that sum to `sum`.

  Returns nil if there is no pair."
  [input sum]
  (letfn [(has-partner [x] (get input (- sum x)))]
    (when-let [first-number (some has-partner input)]
      #{first-number (- sum first-number)})))

(defn solve-task1 [input]
  (let [pair (find-pair input 2020)]
    (apply * pair)))

(defn find-triplet
  "Given a set of `input` numbers and one `candidate`, returns (when possible) a triplet that sums to 2020.
   The `candidate` is one member of the triplet.

   Returns nil if there is no triplet for the given `candidate`"
  [input sum candidate]
  (let [new-input (disj input candidate)
        new-sum (- sum candidate)]
    (when-let [pair (find-pair new-input new-sum)]
      (conj pair candidate))))

(defn solve-task2 [input]
  (let [triplet (some (partial find-triplet input 2020) input)]
    (apply * triplet)))

(println "Answer for star 1: " (time (solve-task1 *input*)))
(println "Answer for star 2: " (time (solve-task2 *input*)))