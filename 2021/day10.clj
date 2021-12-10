#!/usr/bin/env bb
(ns day10
  (:require [clojure.string :as str]))
            
(def input-path (or (first *command-line-args*) "2021/inputs/day10.txt"))

(defn parse-input [input]
  (str/split-lines input))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"))

(def mirror
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(def corrupted-points
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def incomplete-points
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn score [x]
  (if (char? x)
    (corrupted-points x)
    (reduce
      (fn [s p] (+ (* s 5) p))
      0
      (map incomplete-points x))))

(def opening? (set (keys mirror)))

(defn lint
  "Checks a sequence of characters for matching parens.
  If the sequence is:
  corrupted (i.e. a chunk is closing without opening)
    -> Returns the first corrupting character.
  incomplete (i.e. one or more chunks are not closed in the end)
    -> Returns the matching sequence of closing parens.
  valid
    -> Returns nil"
  ([in] (lint in []))
  ([[h & t] stack]
   (if h
     (if (opening? h)
       (recur t (conj stack h))
       ;; closing
       (if (= h (mirror (peek stack)))
         (recur t (pop stack))
         h))
     (when (pos? (count stack))
       (map mirror (reverse stack))))))

;;; Task 1

(defn task1
  {:test #(assert (= 26397 (task1 test-input)))}
  [input]
  (->> input
    (map lint)
    (filter char?)
    (map score)
    (reduce +)))


;;; Task 2  

(defn median [coll]
  (let [i (quot (count coll) 2)]
    (nth (sort coll) i)))
  
(defn task2
  {:test #(assert (= 288957 (task2 test-input)))}
  [input]
  (->> input
    (map lint)
    (remove char?)
    (map score)
    median))


;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))