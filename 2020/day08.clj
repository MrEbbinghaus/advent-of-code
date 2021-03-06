#!/usr/bin/env bb
(ns day08
  (:require [clojure.string :as str]))

(defn parse-line [lint]
  (let [[cmd value] (str/split lint #" ")]
    [cmd (Long/parseLong value)]))

(def *input* (->> *in* slurp str/split-lines (map parse-line) vec))

(defn solve-task1 [instructions]
  (loop [ip 0
         acc 0
         already-run #{}]
    (if (already-run ip)
      acc
      (let [[cmd value] (get instructions ip)]
        (case cmd
          "nop" (recur (inc ip) acc (conj already-run ip))
          "acc" (recur (inc ip) (+ acc value) (conj already-run ip))
          "jmp" (recur (+ ip value) acc (conj already-run ip)))))))

(defn execute
  "Executes a vector of instructions.
  Returns a number, or nil in case of a failure."
  [instructions]
  (loop [ip 0
         acc 0
         already-run #{}]
    (cond
      ;; exactly one after the last instruction
      (= (count instructions) ip)
      acc

      ;; in loop or more then one after the last instruction
      (or (already-run ip) (< (count instructions) ip))
      nil                                                   ; indicate failure

      ;; execute as normal
      :else
      (let [[cmd value] (get instructions ip)]
        (case cmd
          "nop" (recur (inc ip) acc (conj already-run ip))
          "acc" (recur (inc ip) (+ acc value) (conj already-run ip))
          "jmp" (recur (+ ip value) acc (conj already-run ip)))))))

(def replace-cmd
  {"nop" "jmp"
   "jmp" "nop"})

(defn variation
  "Swaps the instruction at position `ip` between 'nop' and 'jmp'."
  [instructions ip]
  (let [[cmd value] (get instructions ip)]
    (assoc instructions ip
      [(replace-cmd cmd cmd) value])))

(defn all-variations [instructions]
  "Return a set of all instructions where at exactly one position a 'nop' replaces a 'jmp' or a 'jmp' replaces a 'nop'."
  (set (map #(variation instructions %) (range (count instructions)))))

(defn solve-task2 [instructions]
  (some execute (all-variations instructions)))

(println "Answer for star 1: " (solve-task1 *input*))
(println "Answer for star 2: " (time (solve-task2 *input*)))