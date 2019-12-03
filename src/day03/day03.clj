(ns day03.day03
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn parse-wire [raw-wire]
  (map #(vector (first %) (edn/read-string (apply str (rest %)))) raw-wire))

(def input (->> "src/day03/input.txt" slurp str/split-lines
             (map (comp parse-wire #(str/split % #",")))))

(defn positions [[x y] [direction steps]]
  (for [i (range 1 (inc steps))]
    [(if (#{\L \R} direction)
       ((if (= \L direction) - +) x i)
       x)
     (if (#{\U \D} direction)
       ((if (= \D direction) - +) y i)
       y)]))

(defn update-grid [grid id distance [position & r]]
  (if position
    (recur (update grid position (fn [m]
                                   (when-not (get m id)
                                     (assoc m id distance))))
      id (inc distance) r)
    grid))

(defn wire-grid
  ([id wire] (wire-grid id wire 1 [0 0] {}))
  ([id wire distance position grid]
   (let [[move] wire]
     (if move
       (let [step-positions (positions position move)]
         (recur id
           (drop 1 wire)
           (+ distance (second move))
           (last step-positions)
           (update-grid grid id distance step-positions)))
       grid))))

(defn d [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn intersections [grid]
  (filter (fn [[_ v]] (> (count v) 1)) grid))

(comment
  "Task 1"
  (->> (merge-with clojure.set/union
         (wire-grid :first (first input))
         (wire-grid :second (second input)))
    intersections
    keys
    (sort-by (partial d [0 0]))
    first
    (d [0 0]))

  "Task 2"
  (->> (merge-with clojure.set/union
         (wire-grid :wire1 (first input))
         (wire-grid :wire2 (second input)))
    intersections
    (map (comp #(reduce + %) vals second))
    (apply min)))

