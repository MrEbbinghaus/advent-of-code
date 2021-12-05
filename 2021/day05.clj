#!/usr/bin/env bb
(ns day05)

(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
          (catch AssertionError e
               (ex-message e)))))

(def input-path (or (first *command-line-args*) "./inputs/day05.txt"))

(defn parse-input [input]
  (->> input
    (re-seq #"\d+")
    (map parse-long)
    (partition 4)
    (map #(split-at 2 %))))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"))

(defn direction [p1 p2]
  (mapv - p2 p1))

(defn scale [v n]
  (mapv #(int (* % n)) v))

(defn add [v1 v2]
  (map + v1 v2))

(defn norm
  ([[x y]] (max (Math/abs ^int x) (Math/abs ^int y)))
  ([origin p] (norm (direction origin p))))

(defn normvector
  ([v] (scale v (/ 1 (norm v))))
  ([origin p] (normvector (direction origin p))))

(defn points-between [p1 p2]
  (let [ev (normvector p1 p2)]
    (map #(add p1 (scale ev %)) (range (inc (norm p1 p2))))))

(defn crossings [input]
  (->> input
    (mapcat #(apply points-between %))
    frequencies
    (remove (comp #{1} second))))

;;; Task 1
(defn diagonal? [p1 p2]
  (let [[x y] (normvector p1 p2)]
    (not (or (zero? x) (zero? y)))))

(defn remove-diagonals [input]
  (remove #(apply diagonal? %) input))

(defn task1
  {:test #(assert (= 5 (task1 test-input)))}
  [input]
  (count (crossings (remove-diagonals input))))

;;; Task 2
(defn task2
  {:test #(assert (= 12 (task2 test-input)))}
  [input]
  (count (crossings input)))


(all-tests)

(defn print-map [input]
  (let [positions
        (->> input
          (mapcat #(apply points-between %))
          frequencies)
        not-empty-points (keys positions)
        min-x (apply min (map first not-empty-points))
        max-x (apply max (map first not-empty-points))
        min-y (apply min (map second not-empty-points))
        max-y (apply max (map second not-empty-points))]
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (print (get positions [x y] ".")))
      (println))))

;;; Print output
(print "Task 1: ")
(println (task1 input))
(print "Task 2: ")
(println (task2 input))