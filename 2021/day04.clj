#!/usr/bin/env bb
(ns day04
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.pprint :as pprint]))

(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
          (catch AssertionError e
               (ex-message e)))))

(def input-path (or (first *command-line-args*) "2021/inputs/day04.txt"))

(defn parse-input [input]
  (let [[draws & board-numbers] (str/split input #"\s+")
        drawn-numbers (map parse-long (str/split draws #","))
        boards (->> board-numbers
                 (map parse-long)
                 (partition 25)
                 (map vec))]
     {:draws drawn-numbers}
      :boards (map #(hash-map :fields % :marks #{}) boards)))

(def test-string "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")
(def test-input (parse-input test-string))
(def test-board {:fields [22 13 17 11 0
                          8 2 23 4 24
                          21 9 14 16 7
                          6 10 3 18 5
                          1 12 20 15 19]
                 :marks #{}})
(def input (parse-input (slurp input-path)))

(defn five-of [component vecs]
  (some #(= 5 %) (vals (frequencies (map component vecs)))))

(defn win?
  {:test #(do
            (assert (win? {:marks #{[0 0] [0 1] [0 2] [0 3] [0 4]}}))
            (assert (win? {:marks #{[0 0] [1 0] [2 0] [3 0] [4 0]}}))
            (assert (not (win? test-board))))}
  [board]
  (let [marks (:marks board)]
    (or
      (five-of first marks)
      (five-of second marks))))

(defn coords-of
  {:test #(assert (= [2 2] (coords-of test-board 14)))}
  [board number]
  (when-let [index (get (set/map-invert (:fields board)) number)]
    [(quot index 5) (rem index 5)]))

(defn coords->index [[x y]]
  (+ (* 5 x) y))

(defn mark
  {:test #(do
            (assert (= #{[2 2]} (:marks (mark test-board 14)))))}
  [board number]
  (if-let [coords (coords-of board number)]
    (update board :marks conj coords)
    board))

(defn marked [board]
  (set (vals (select-keys (:fields board) (map coords->index (:marks board))))))

(defn unmarked [board]
  (set/difference (set (:fields board)) (marked board)))


(def col-header "BINGO")
(defn print-board [board]
  (->> board
    :fields
    (map #(cond-> % ((marked board) %)) (format "(%d)"))
    (map #(if ((marked board) %) (str "(" % ")") %))
    (partition 5)
    (map #(zipmap col-header %))
    (pprint/print-table col-header)))

(defn print-result [board draw]
  (print "Final board:")
  (print-board board)
  (println (str/join " + " (unmarked board)) "=" (reduce + (unmarked board)))
  (println "Last draw:" draw))

;;; Task 1

(defn task1
  {:test #(assert (= 4512 (task1 test-input)))}
  [input]
  (loop [[current-draw & draws] (:draws input)
         boards (:boards input)]
    (when current-draw
      (let [new-boards (map #(mark % current-draw) boards)]
        (if-let [winner (first (filter win? new-boards))]
          (* current-draw (reduce + (unmarked winner)))
          (recur draws new-boards))))))

;;; Task 2

(defn task2
  {:test #(assert (= 1924 (task2 test-input)))}
  [input]
  (loop [[current-draw & draws] (:draws input)
         boards (:boards input)]
    (when current-draw
      (let [new-boards (map #(mark % current-draw) boards)]
        (if (and (= 1 (count new-boards)) (win? (first new-boards)))
          (* current-draw (reduce + (unmarked (first new-boards))))
          (recur draws (remove win? new-boards)))))))

(all-tests)


;;; Print output
(print "Task 1: ")
(println (task1 input))
(print "Task 2: ")
(println (task2 input))