#!/usr/bin/env bb
(ns day05
  (:require [clojure.string :as str]))
  
(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
       (catch AssertionError e
          (ex-message e)))))
            
(def input-path (or (first *command-line-args*) "2022/inputs/day05.txt"))
(def test-input-str "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2")

(defn parse-line
  "Parses a move from a string."
  [line]
  (let [[_ move from to] (re-find #"move (\d+) from (\d+) to (\d+)" line)]
    {:move (parse-long move) :from (dec (parse-long from)) :to (dec (parse-long to))}))

(defn parse-board-state
  "Parses the initial state of the game from a string."
  [initial-state-str]
  (->> initial-state-str
       butlast ; get rid of stack numbers
       (apply mapv vector) ; transpose
       rest ; get rid of first space line
       (take-nth 4)
       (mapv #(apply list (remove #{\space} %)))))

(defn parse-input
  "Parses the input for the game."
  [input]
  (let [[initial moves] (split-with (complement #{""}) (str/split-lines input))]
    {:initial (parse-board-state initial)
     :moves (map parse-line (rest moves))}))

(def input (parse-input (slurp input-path)))


;;; Task 1
(defn move-top-block
  "Moves the top block from one stack to another."
  [state from to]
  (let [to-move (peek (get state from))]
    (-> state
        vec
        (update from pop)
        (update to conj to-move))))

(defn apply-single-move
  "Applies a single move to the current state of the game."
  [state {:keys [move from to]}]
  (nth (iterate #(move-top-block % from to) state) move))

(defn task1
  "Solves the first task for the game."
  [{:keys [initial moves]}]
  (transduce (map first) str "" (reduce apply-single-move initial moves)))

  
;;; Task 2
(defn apply-multi-move
  "Applies a multi-block move to the current state of the game."
  [state {:keys [move from to]}]
  (let [to-move (take move (get state from))]
    (-> state
        (update from #(drop move %))
        (update to #(concat to-move %)))))

(defn task2
  "Solves the second task for the game."
  [{:keys [initial moves]}]
  (transduce (map first) str "" (reduce apply-multi-move initial moves)))

;;; Print output
;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))