#!/usr/bin/env bb
(ns day13
  (:require [clojure.string :as str]))

(defn parse [[earliest-departure busses]]
  {:earliest-departure (Long/parseLong earliest-departure)
   :bus-ids (map #(Long/parseLong %) (re-seq #"\d+" busses))})

(def *input (->> *in* slurp str/split-lines parse))

(defn multiples-of [x]
  (map * (range) (repeat x)))

(defn get-earliest-departure-after [departure-time bus-id]
  (first (drop-while #(< % departure-time) (multiples-of bus-id))))

(defn solve-task1 [{:keys [earliest-departure bus-ids]}]
  (let [[bus-id min-to-depart]
        (->> bus-ids
          (map #(vector % (get-earliest-departure-after earliest-departure %)))
          (sort-by second)
          first)
        min-to-wait (- min-to-depart earliest-departure)]
    (* bus-id min-to-wait)))


(println "Answer for star 1: " (time (solve-task1 *input)))