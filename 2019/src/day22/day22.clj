(ns day22.day22
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest is are]]))

(def my-input (slurp "2019/src/day22/input.txt"))

(defn type [line]
  (case (get line 5)
    \i :new
    \w :increment
    :cut))

(defn parse-line [line]
  {:type  (type line)
   :value (some->> line (re-find #"(-?\d+)$") second Integer/parseInt)})

(defn parse-input [input]
  (->> input str/split-lines (map parse-line)))


(defmulti deck-shuffle :type)

(defmethod deck-shuffle :new
  [_ deck]
  (reverse deck))

(defmethod deck-shuffle :cut
  [{:keys [value]} deck]
  (let [deck-size (count deck)]
    (let [[front back] (split-at (mod (+ deck-size value) deck-size)  deck)]
      (concat back front))))

(defmethod deck-shuffle :increment
  [{:keys [value]} deck]
  (let [deck-size (count deck)
        card-positions (map #(mod % deck-size) (range 0 (* value deck-size) value))]
    (->> deck
      (map vector card-positions)
      (sort-by first)
      (map second))))

(defn run-commands [deck commands]
  (reduce #(deck-shuffle %2 %1) deck commands))

(defn solve-part1 [input]
  (get (zipmap (run-commands (range 10007) (parse-input input)) (range)) 2019))

(time (solve-part1 (slurp "2019/src/day22/input.txt")))

(are [x y] (= x y)
  [9 8 7 6 5 4 3 2 1 0] (deck-shuffle (parse-line "deal into new stack") (range 10))
  [3 4 5 6 7 8 9 0 1 2] (deck-shuffle (parse-line "cut 3") (range 10))
  [6 7 8 9 0 1 2 3 4 5] (deck-shuffle (parse-line "cut -4") (range 10))
  [0 7 4 1 8 5 2 9 6 3] (deck-shuffle (parse-line "deal with increment 3") (range 10))
  [0 3 6 9 2 5 8 1 4 7] (run-commands (range 10) (map parse-line
                                                   ["deal with increment 7"
                                                    "deal into new stack"
                                                    "deal into new stack"]))
  [3 0 7 4 1 8 5 2 9 6] (run-commands (range 10) (map parse-line
                                                   ["cut 6"
                                                    "deal with increment 7"
                                                    "deal into new stack"]))
  [6 3 0 7 4 1 8 5 2 9] (run-commands (range 10) (map parse-line
                                                   ["deal with increment 7"
                                                    "deal with increment 9"
                                                    "cut -2"]))
  [9 2 5 8 1 4 7 0 3 6] (run-commands (range 10) (map parse-line
                                                   ["deal into new stack"
                                                    "cut -2"
                                                    "deal with increment 7"
                                                    "cut 8"
                                                    "cut -4"
                                                    "deal with increment 7"
                                                    "cut 3"
                                                    "deal with increment 9"
                                                    "deal with increment 3"
                                                    "cut -1"])))