#!/usr/bin/env bb
(ns day02
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (map #(let [[cmd val] (str/split % #" ")]
          [(keyword cmd) (Integer/parseInt val)])
    (str/split-lines input)))

(def input (parse-input (slurp (first *command-line-args*))))

(defn input->vec2
  [[cmd val]]
  (case cmd
    :forward [val 0]
    :down [0 val]
    :up [0 (- val)]))

(defn task1 [input]
  (let [start-position [0 0]] ; y, depth
    (->> input
      (map input->vec2)
      (reduce #(mapv + %1 %2) start-position)
      (reduce *))))

(defn assert-moves [f moves]
  (loop [position [0 0 0]
         [[cmd expected] & rest-moves] moves]
    (when cmd
      (assert (= expected (f position cmd)))
      (recur expected rest-moves))))

(defn move-sub
  {:test #(assert-moves move-sub
            [[[:forward 5] [5 0 0]]
             [[:down 5] [5 0 5]]
             [[:forward 8] [13 40 5]]])}
  [position [cmd val]]
  (let [[y depth aim] position]
    (case cmd
      :forward [(+ y val) (+ depth (* val aim)) aim]
      :down [y depth (+ aim val)]
      :up [y depth (- aim val)])))

(defn task2 [input]
  (let [start-position [0 0 0] ; y, depth, aim
        [final-y final-depth] (reduce move-sub start-position input)]
    (* final-y final-depth)))


(print "Task 1: ")
(println (task1 input))
(print "Task 2: ")
(println (task2 input))