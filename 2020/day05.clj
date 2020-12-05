#!/usr/bin/env bb
(ns day05
  (:require
    [clojure.string :as str]))

(def *input* (-> *in* slurp str/split-lines))

(defn get-row-and-col [boarding-pass]
  (let [row-instructions (take 7 boarding-pass)
        col-instructions (drop 7 boarding-pass)]
    (mapv first
      [(reduce
         (fn [[from to] instruction]
           (let [half (+ from (quot (- to from) 2))]
             (case instruction
               \F [from half]
               \B [(inc half) to])))

         [0 127] row-instructions)

       (reduce
         (fn [[from to] instruction]
           (let [half (+ from (quot (- to from) 2))]
             (case instruction
               \L [from half]
               \R [(inc half) to])))

         [0 7] col-instructions)])))

(defn seat-id [[row col]]
  (-> 8 (* row) (+ col)))

(defn solve-task1 [input]
  (->> input
    (map (comp seat-id get-row-and-col))
    (reduce max)))

(def highest-seat-id (seat-id [127 7]))
(def all-possible-seat-ids (set (range highest-seat-id)))

;; Do not judge it is my weekend
(defn solve-task2 [input]
  (->> input
    (map (comp seat-id get-row-and-col))                    ; get all seat-ids in input
    (apply disj all-possible-seat-ids)              ; disjoin them from all possible seat ids to get the missing ones
    sort
    (partition 3 1)                                         ; partner them up with the next seat-ids
    (remove (fn [[x y z]]
              (or (= x (dec y))
                (= z (inc y)))))                            ; check if the either partner is also missing, if so remove this triplet
    first second))                                          ; get the final answer

(println "Answer for star 1: " (solve-task1 *input*))
(println "Answer for star 1: " (solve-task2 *input*))