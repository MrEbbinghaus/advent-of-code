#!/usr/bin/env bb
(ns day11)
            
(def input-path (or (first *command-line-args*) "2021/inputs/day11.txt"))

(defn parse-digit [char]
  (- (short char) (short \0)))

(defn parse-input [input]
  (into []
    (comp
      (remove #{\newline})
      (map parse-digit)
      (partition-all 10))
    (seq input)))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"))

(def all-coords (for [x (range 10) y (range 10)] [x y]))

(defn in-bounds? [[x y]]
  (and
    (<= 0 x 9)
    (<= 0 y 9)))

(def points-around
  (memoize
    (fn [[x y]]
      (filter in-bounds?
        [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
         [(dec x) y]        #_[x y]          [(inc x) y]
         [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]))))

(defn map-cave [f cave]
  (into []
    (comp
      cat
      (map f)
      (partition-all 10))
    cave))

(defn will-flash? [octopus]
  (< 9 octopus))

(defn about-to-flash? [octopus]
  (= 9 octopus))

(defn flash-coords [cave]
  (keep
    #(when (will-flash? (get-in cave %)) %)
    all-coords))

;;; Task 1

(defn step
  "Returns [new-cave flashes]"
  [cave]
  (let [charged-cave (map-cave inc cave)]
    (loop [cave charged-cave
           [pos & r] (mapcat points-around (flash-coords cave))]
      (if pos
        (let [octopus (get-in cave pos)
              new-cave (assoc-in cave pos (inc octopus))]
          (if (about-to-flash? octopus)
            (recur new-cave (concat r (points-around pos)))
            (recur new-cave r)))
        (map-cave
          #(if (will-flash? %) 0 %)
          cave)))))

(defn count-flashes [cave]
  (count (filter zero? (flatten cave))))


(defn task1
  {:test #(assert (= 1656 (task1 test-input)))}
  [input]
  (->> input
    (iterate step)
    rest ; don't consider initial state
    (take 100)
    (map count-flashes)
    (reduce +)))

;;; Task 2

(defn synchronized? [cave]
  (every? zero? (flatten cave)))
  
(defn task2
  {:test #(assert (= 195 (task2 test-input)))}
  [input]
  (->> input
    (iterate step)
    (take-while (complement synchronized?))
    count))

;;; Print output
(println "Task 1:" (time (task1 input)))
(println "Task 2:" (time (task2 input)))