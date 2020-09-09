(ns day01.day01
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (->> "src/day01/input.txt" slurp str/split-lines (map edn/read-string)))

(defn fuel-for-module [mass]
  (-> mass (quot 3) (- 2)))

(defn fuel-for-module2
  ([mass] (fuel-for-module2 0 mass))
  ([current-fuel mass]
   (let [fuel (fuel-for-module mass)]
     (if (<= fuel 0)
       current-fuel
       (recur (+ current-fuel fuel) fuel)))))

(comment
  "Task 1"
  (reduce + (map fuel-for-module input))

  "Task 2"
  (reduce + (map fuel-for-module2 input)))