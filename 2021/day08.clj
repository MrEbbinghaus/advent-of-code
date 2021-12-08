#!/usr/bin/env bb
(ns day08
  (:require
    [clojure.set :as set]))

(def input-path (or (first *command-line-args*) "2021/inputs/day08.txt"))

(defn parse-input [input]
  (->> input
    (re-seq #"\w+")
    (map #(into #{} %))
    (partition 14)
    (map #(split-at 10 %))))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |\nfdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |\nfcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |\ncg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |\nefabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |\ngecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |\ngebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |\ncefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |\ned bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |\ngbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |\nfgae cfgab fg bagce"))
(def test-entry (first (parse-input "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |\ncdfeb fcadb cdfeb cdbaf")))

;;  aaa
;; b   c
;; b   c
;;  ddd
;; e   f
;; e   f
;;  ggg

;;; Task 1
(defn task1
  {:test #(assert (= 26 (task1 test-input)))}
  [input]
  (->> input
    (mapcat second)
    (map count)
    (filter #{2 3 4 7})
    count))

(defn conj-number [number digit]
  (+ (* 10 number) digit))

(defn process-entry
  {:test #(assert (= 5353 (process-entry test-entry)))}
  [[hints output]]
  (let [;; one has 2 segments, seven 3 and fours has 4
        [one seven four & more] (sort-by count hints)
        ;; There are 3 Digits with 5 segments, 3 Digits with 6 segments and 1 Digit with 7.
        [contains235 contains069 [eight]] (partition-all 3 more)

        intersects-with-4 #(count (set/intersection four %))
        difference-from-1 #(count (set/difference one %))

        ;; [2, 3, 5] ∩ 4 -> 2, 3, 3
        ;; [2, 3 ,5] / 1 -> 4, 3, 4
        [two three five]
        (sort-by (juxt intersects-with-4 difference-from-1) contains235)

        ;; [0, 6, 9] ∩ 4 -> 3, 3, 4
        ;; [0, 6 ,0] / 1 -> 2, 1, 0
        [zero six nine]
        (sort-by (juxt intersects-with-4 difference-from-1) contains069)

        to-value
        {zero 0
         one 1
         two 2
         three 3
         four 4
         five 5
         six 6
         seven 7
         eight 8
         nine 9}]
    (reduce conj-number (map to-value output))))



(defn task2
  {:test #(assert (= 61229 (task2 test-input)))}
  [input]
  (reduce + (map process-entry input)))


;;; Print output
(print "Task 1: ")
(println (task1 input))
(print "Task 2: ")
(println (time (task2 input)))