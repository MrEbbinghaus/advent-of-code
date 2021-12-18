#!/usr/bin/env bb
(ns day18
  (:require
    [clojure.string :as str]
    [clojure.edn :as edn]
    [clojure.zip :as z]))
            
(def input-path (or (first *command-line-args*) "2021/inputs/day18.txt"))

(defn parse-input [input-str]
  (->> input-str
    str/split-lines
    (map edn/read-string)))

(def input (parse-input (slurp input-path)))
(def test-input
  [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
   [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
   [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
   [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
   [7,[5,[[3,8],[1,4]]]]
   [[2,[2,2]],[8,[8,1]]]
   [2,9]
   [1,[[[9,3],9],[[9,0],[0,7]]]]
   [[[5,[7,4]],7],1]
   [[[[4,2],2],6],[8,7]]])

(defn next-leaf [loc]
  (loop [loc (z/next loc)]
    (when-not (z/end? loc)
      (if (z/branch? loc)
        (recur (z/next loc))
        loc))))

(defn prev-leaf [loc]
  (loop [loc (z/prev loc)]
    (when loc
      (if (z/branch? loc)
        (recur (z/prev loc))
        loc))))

(defn depth [[_ path]]
  (loop [path path
         depth 0]
    (if path
      (recur (:ppath path) (inc depth))
      depth)))

(defn first-exploding-node
  [loc]
  (when-not (z/end? loc)
    (if (and (< 3 (depth loc)) (z/branch? loc))
      loc
      (recur (z/next loc)))))

(defn first-split-node [loc]
  (when-not (z/end? loc)
    (if (and (not (z/branch? loc)) (< 9 (z/node loc)))
      loc
      (recur (z/next loc)))))

(defn split [loc]
  (z/edit loc #(vector (quot % 2) (int (Math/ceil (/ % 2))))))

(defn explode [loc]
  (let [[[l r] _] loc
        loc (z/replace loc 0)
        ;; need some kind of cond-let-> macro..
        loc (if-let [p (prev-leaf loc)]
              (next-leaf (z/edit p + l))
              loc)
        loc (if-let [n (next-leaf loc)]
              (prev-leaf (z/edit n + r))
              loc)]
    loc))

(defn reduce-number-once [number]
  (let [zipper (z/vector-zip number)]
    (if-let [exploding-loc (first-exploding-node zipper)]
      (z/root (explode exploding-loc))
      (when-let [split-loc (first-split-node zipper)]
        (z/root (split split-loc))))))


(defn reduce-number
  {:test #(do
            (assert (= [[6,[5,[7,0]]],3] (reduce-number [[6,[5,[4,[3,2]]]],1])))
            (assert (= [[3,[2,[8,0]]],[9,[5,[7,0]]]] (reduce-number [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]))))}
  [number]
  (if-let [reduced-number (reduce-number-once number)]
    (recur reduced-number)
    number))

(defn add [x y]
  (reduce-number [x y]))

;;; Task 1
(defn magnitude [number]
  (if (number? number)
    number
    (+ (* 3 (magnitude (number 0)))
       (* 2 (magnitude (number 1))))))

(defn task1 [input]
  (magnitude (reduce add input)))

;;; Task 2

(def test-input2
  [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
   [[[5,[2,8]],4],[5,[[9,9],0]]]
   [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
   [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
   [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
   [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
   [[[[5,4],[7,7]],8],[[8,3],8]]
   [[9,3],[[9,9],[6,[4,9]]]]
   [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
   [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]])
  
(defn task2 [input]
  (apply max
    (for [x input
          y input
          :when (not= x y)]
      (magnitude (add x y)))))


;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))