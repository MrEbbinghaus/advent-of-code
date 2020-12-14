#!/usr/bin/env bb
(ns day14
  (:require [clojure.string :as str]))

(def mask-pattern #"^mask = ([X10]{36})$")
(def memory-pattern #"^mem\[(\d+)\] = (\d+)$")

(defmulti parse-line second)
(defmethod parse-line \a [line]
  (let [[_ mask] (re-find mask-pattern line)]
    {:type :mask
     :mask mask}))

(defmethod parse-line \e [line]
  (let [[_ location value] (re-find memory-pattern line)]
    {:type :memory
     :location (Long/parseLong location)
     :value (Long/parseLong value)}))

(def *input (->> *in* slurp str/split-lines (map parse-line)))

(defn int->36bit [x]
  (take-last 36 (concat (repeat 36 \0) (Long/toBinaryString x))))

(defn apply-mask [mask value]
  (let [bin-value (int->36bit value)]
    (Long/parseUnsignedLong
      (apply str (map #(if (#{\0 \1} %1) %1 %2) mask bin-value))
      2)))

(defmulti execute #(:type %2))

(defmethod execute :mask
  [memory {:keys [mask]}]
  (assoc memory ::mask mask))

(defmethod execute :memory
  [{::keys [mask]
    :as memory} {:keys [location value]}]
  (let [value (apply-mask mask value)]
    (assoc memory location value)))

(defn run [input]
  (reduce
    execute
    {} ; initial memory
    input))

(defn solve-task1 [input]
  (let [memory (run input)]
    (->> (dissoc memory ::mask)
      vals
      (reduce +))))

(defn apply-flip [mask bin-str]
  (map #(if (#{\1 \X} %1) %1 %2) mask bin-str))

(defn get-memory-addresses [mask memory-address]
  (let [bin-value (apply-flip mask (int->36bit memory-address))
        no-of-X (get (frequencies mask) \X)
        format-string (apply str (replace {\X "%c"} bin-value))]
    (for [x (range (Math/pow 2 no-of-X))
          :let [bla (take-last no-of-X (concat (repeat 36 \0) (Long/toBinaryString x)))]]
      (apply (comp #(Long/parseUnsignedLong % 2) format) format-string bla))))

(defmulti execute2 #(:type %2))

(defmethod execute2 :mask
  [memory {:keys [mask]}]
  (assoc memory ::mask mask))

(defmethod execute2 :memory
  [{::keys [mask]
    :as memory} {:keys [location value]}]
  (apply assoc memory (interleave (get-memory-addresses mask location) (repeat value))))

(defn run2 [input]
  (reduce
    execute2
    {} ; initial memory
    input))

(defn solve-task2 [input]
  (let [memory (run2 input)]
    (->> (dissoc memory ::mask)
      vals
      (reduce +))))

(println "Answer for star 1: " (time (solve-task1 *input)))
(println "Answer for star 12: " (time (solve-task2 *input)))