(ns day04.day04)

(def input [273025 767253])

(defn long->digits [x]
  (seq (str x)))

(defn has-double? [d]
  (-> d dedupe count (not= 6)))

(defn has-exactly-double? [d]
  (->> d frequencies vals (some #{2}) some?))

(defn not-decreasing? [d]
  (= (sort d) d))

(comment
  "Task 1"
  (->> input
    (apply range)
    (map long->digits)
    (filter (every-pred has-double? not-decreasing?))
    count)

  "Task 2"
  (->> input
    (apply range)
    (map long->digits)
    (filter (every-pred not-decreasing? has-exactly-double?))
    count))