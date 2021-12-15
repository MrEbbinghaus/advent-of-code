#!/usr/bin/env bb
(ns day14
  (:require [clojure.string :as str]))

            
(def input-path (or (first *command-line-args*) "2021/inputs/day14.txt"))

(defn parse-rule [rule]
  (let [[pattern insert] (rest (re-find #"([A-Z]{2}) -> ([A-Z])" rule))]
    [(seq pattern) (first insert)]))

(defn parse-input [input]
  (let [[template & rules] (str/split input #"\n+")]
    {:template (seq template)
     :rules (->> rules
              (map parse-rule)
              (into {}))}))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"))


(defn insert-frequencies
  {:test #(do
            (assert (= {[\A \B] 1, [\B \B] 1}
                      (insert-frequencies {[\A \B] \B, [\B \B] \B}
                                          {[\A \B] 1}))))}
  [rules pair-frequencies]
  (transduce
    (map (fn [[a b]]
           (let [insert (rules [a b])]
             {[a insert] (pair-frequencies [a b])
              [insert b] (pair-frequencies [a b])})))
    (partial merge-with +)
    (keys pair-frequencies)))

(defn pairs [input]
  (partition 2 1 (:template input)))

(defn insert-times
  [n input]
  (nth (iterate
         (partial insert-frequencies (:rules input))
         (frequencies (pairs input))) n))

(defn compute [input n]
  (let [inserted-template (insert-times n input)

        component-frequencies
        (reduce-kv
          (fn [m [f _] v]
            (update m f (fnil + 0) v))

          {(last (:template input)) 1}
          inserted-template)

        y (sort (vals component-frequencies))]
    (- (last y) (first y))))

(defn task1
  {:test #(assert (= 1588 (task1 test-input)))}
  [input]
  (compute input 10))

(defn task2
  {:test #(assert (= 2188189693529 (task2 test-input)))}
  [input]
  (compute input 40))

;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))