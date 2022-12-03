#!/usr/bin/env bb
(ns day02
  (:require [clojure.string :as str]))
  
(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
       (catch AssertionError e
          (ex-message e)))))
            
(def input-path (or (first *command-line-args*) "2022/inputs/day02.txt"))



(defn parse-line [line]
  [(first line) (nth line 2)])

(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(def input (parse-input (slurp input-path)))

(def move->rating
  {:rock 1
   :paper 2
   :scissors 3})

(defn win? [[opponent you]]
  (case opponent
    :rock (= :paper you)
    :paper (= :scissors you)
    :scissors (= :rock you)))

(defn draw? [[opponent you]]
  (= opponent you))

(defn loss?  [match]
  (not (or (draw? match) (win? match))))

(defn rate
  {:test #(do
            (assert (= 8 (rate [:rock :paper])))
            (assert (= 1 (rate [:paper :rock])))
            (assert (= 6 (rate [:scissors :scissors]))))}
  [match]
  (+
    (move->rating (second match))
    (cond
      (win? match) 6
      (draw? match) 3
      (loss? match) 0)))

(def letter->move-name
  {\A :rock
   \B :paper
   \C :scissors
   \X :rock
   \Y :paper
   \Z :scissors})

;;; Task 1
(defn task1 [matches]
  (->> matches
       (map #(mapv letter->move-name %))
       (map rate)
       (apply +)))


  
;;; Task 2  
(def ratings
  {\A
   {\X 3
    \Y 4
    \Z 8}
   \B
   {\X 1
    \Y 5
    \Z 9}
   \C
   {\X 2
    \Y 6
    \Z 7}})

  
(defn task2 [input]
  (transduce
    (map #(get-in ratings %))
    + 0
    input))


;;; Print output
;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))