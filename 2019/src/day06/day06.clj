(ns day06.day06
  (:require [clojure.string :as str]))

(def input (map #(str/split % #"\)") (-> "src/day06/input.txt" slurp (str/split-lines))))
(def rev-input (into {} (map (comp vec reverse)) input))

(def distance-to-top
  (memoize
    (fn [rev-input node]
      (if-let [next (rev-input node)]
        (inc (distance-to-top rev-input next))
        0))))

(defn path-to-top [rev-input node]
  (if-some [next (rev-input node)]
    (conj (path-to-top rev-input next) node)
    [node]))

(comment
  "Task 1"
  (->> input
    (into #{} cat)
    (map (partial distance-to-top rev-input))
    (reduce +))

  "Task 2"
  (let
    [you-path (path-to-top rev-input "YOU")
     san-path (path-to-top rev-input "SAN")
     common (some (set you-path) (reverse san-path))
     not-common (complement #{common})]
    (+ -4 ; -1 common node -2 YOU/SAN nodes -1 to get jumps between nodes
      (count (drop-while not-common you-path))
      (count (drop-while not-common san-path)))))


