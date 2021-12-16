#!/usr/bin/env bb
(ns day15
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

            
(def input-path (or (first *command-line-args*) "2021/inputs/day15.txt"))

(defn parse-input [input-str]
  (let [lines (str/split-lines input-str)]
    (mapv
      (fn parse-line [line]
        (mapv #(Character/digit ^char % 10) line))
      lines)))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"))

(defn neighbours [[x y]]
  [           [x (dec y)]
   [(dec x) y]           [(inc x) y]
              [x (inc y)]])

(defn to-path [previous start end]
  (loop [end end
         path []]
    (if (= start end)
      (conj path end)
      (recur (previous end) (conj path end)))))

(defn Dijkstra [cost start end]
  (loop [nodes (set (for [x (range (count (first cost)))
                          y (range (count cost))]
                         [x y]))
         distance {start 0}
         previous {}]
    (when (zero? (mod (count nodes) 1000))
      (println "Nodes left:" (count nodes)))
    (if (empty? nodes)
      previous
      (let [nearest-nodes
            (sort-by (fn [node]
                       (distance node ##Inf))
              (set/intersection nodes (set (keys distance))))
            current (first nearest-nodes)
            neighbours-coords (filter nodes (neighbours current))
            new-distances
            (transduce
              (map
                (fn [coord]
                  (let [new-distance (+ (distance current) (get-in cost coord))]
                    (if (< new-distance (distance coord ##Inf))
                      {coord new-distance}
                      {}))))
              merge
              neighbours-coords)]
        (if (= end current)
          previous
          (recur
            (disj nodes current)
            (-> distance
              (dissoc current)
              (merge new-distances))
            (merge previous (zipmap (keys new-distances) (repeat current)))))))))

(defn print-map [map path]
  (let [path-nodes (set path)]
    (doseq [x (range (count (first map)))]
      (doseq [y (range (count map))
              :let [node [x y]]]
        (print
          (str " "
            (if (contains? path-nodes node)
              "·"
              (get-in map node)))))
      (println))))

(defn shortest-path
  ([cost] (shortest-path cost Dijkstra))
  ([cost algorithm]
   (let [start [0 0]
         end [(dec (count (first cost)))
              (dec (count cost))]]
     (reverse (to-path (algorithm cost start end) start end)))))

(defn path-risk [risks path]
  (transduce (map #(get-in risks %)) + path))

;;; Task 1
(defn task1
  {:test #(assert (= 40 (task1 test-input)))}
  [input]
  (let [path (shortest-path input)]
    (print-map input path)
    (path-risk input (rest path))))


(comment
  (profile {}
    (print-map input (shortest-path input Dijkstra)))

  (time (shortest-path input Dijkstra))
  (time (shortest-path input A*))


  ,)

(defn scale [risks scale]
  (mapv
    (fn [line]
      (mapv #(inc (mod (dec (+ % scale)) 9)) line))
    risks))

(defn enlarge [risks]
  (vec
    (apply concat
      (for [sy (range 5)]
        (scale
          (apply mapv (comp vec concat)
            (for [sx (range 5)]
              (scale risks sx)))
          sy)))))

;;; Task 2  
  
  
(defn task2
  {:test #(assert (= 315 (task2 test-input)))}
  [input]
  (let [input (enlarge input)
        path (shortest-path input)]
    (spit "large-maze.txt" (with-out-str (print-map input path)))
    (path-risk input (rest path))))



;;; Print output
;;; Print output
#_(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))