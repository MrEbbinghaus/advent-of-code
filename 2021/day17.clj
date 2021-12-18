#!/usr/bin/env bb
(ns day17)
  
(defn all-tests []
  (update-vals
    (->> *ns* ns-interns (filter (comp :test meta second)))
    #(try (test %)
       (catch AssertionError e
          (ex-message e)))))
            
(def input-path (or (first *command-line-args*) "2021/inputs/day17.txt"))

(def input-pattern #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)")

(defn parse-input [input-str]
  (let [[x-left x-right y-bottom y-top] (map parse-long (rest (re-find input-pattern input-str)))]
    {:x-left x-left
     :x-right x-right
     :y-top y-top
     :y-bottom y-bottom}))

(def input (parse-input (slurp input-path)))
(def test-input (parse-input "target area: x=20..30, y=-10..-5"))


(defn inverse-gauss [sum]
  (- (Math/sqrt (+ (* 2 sum) 1/4)) 1/2))


(defn step-velocity [[x y]]
  [(if (pos? x) (dec x) x)
   (dec y)])

(defn step-position [position velocity]
  (mapv + position velocity))


(defn calculate-trajectory [{:keys [x-right y-bottom]} v-start]
  (loop [[x y] [0 0]
         velocity v-start
         trajectory []]
    (if (or (< x-right x) (< y y-bottom))
      trajectory
      (recur
        (step-position [x y] velocity)
        (step-velocity velocity)
        (conj trajectory [x y])))))


;;; Task 1
(defn max-y [input]
  (- -1 (:y-bottom input)))

(defn task1 [input]
  (let [max-y (- -1 (:y-bottom input))
        best-x (int (inverse-gauss (:x-right input)))]
    (->> [best-x max-y]
      (calculate-trajectory input)
      (map second)
      (apply max))))



;;; Task 2
;; its 2am ... time to go to bed
(defn in-bounds? [{:keys [x-left y-top]} [x y]]
  (and
    (<= x-left x)
    (<= y y-top)))

;; I am sure you can narrow it down even more...
(defn task2 [input]
  (let [max-y (- -1 (:y-bottom input))
        max-x (:x-right input)
        min-y (:y-bottom input)
        min-x (int (Math/ceil (inverse-gauss (:x-left input))))]
    (count
      (for [x (range min-x (inc max-x))
            y (range min-y (inc max-y))
            :let [p (last (calculate-trajectory input [x y]))]
            :when (in-bounds? input p)]
        [x y]))))

;;; Print output
(println "Task 1:" (task1 input))
(println "Task 2:" (task2 input))