#!/usr/bin/env bb
(ns day02)

(defn parse-line [[_ min max [letter] password]]
  {:min (Integer/parseInt min)
   :max (Integer/parseInt max)
   :letter letter
   :password password})

(def *input* (->> *in* slurp (re-seq #"(\d+)-(\d+)\ ([a-z]{1}):\ ([a-z]+)") (pmap parse-line)))

(defn password-correct? [{:keys [min max letter password]}]
  (let [letter-frequencies (frequencies password)]
    (<= min (letter-frequencies letter 0) max)))

(defn solve-task1 [input]
  (count (filter password-correct? input)))

(defn password-correct-2? [{:keys [min max letter password]}]
  (not=
    (= letter (get password (dec min)))
    (= letter (get password (dec max)))))

(defn solve-task2 [input]
  (count (filter password-correct-2? input)))

(println "Answer for star 1: " (time (solve-task1 *input*)))
(println "Answer for star 1: " (time (solve-task2 *input*)))
