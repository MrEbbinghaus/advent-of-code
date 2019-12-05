(ns day05.day05
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (mapv edn/read-string (-> "src/day05/input.txt" slurp (str/split #","))))

(def arity {1 2
            2 2
            3 0
            4 1
            5 2
            6 2
            7 2
            8 2})

(defn destruct-op [op]
  [(mod op 100)
   (concat (->> op str reverse (drop 2)) (repeat \0))])

(defn get-value [input p mode]
  (if (= \1 mode)
    p
    (get input p)))

(def fun
  {1 +
   2 *
   3 #(edn/read-string (read-line))
   4 println
   5 :dont-care
   6 :dont-care
   7 (fn [x y] (if (< x y) 1 0))
   8 (fn [x y] (if (= x y) 1 0))})

(defn run
  ([input] (run 0 (vec input)))
  ([ip input]
   (let [pos-input (drop ip input)
         [op modes] (destruct-op (first pos-input))]
     (if (= op 99)
       (first input)
       (let [ps        (vec (take (arity op) (drop 1 pos-input)))
             params    (map (partial get-value input) ps modes)
             r*        (nth pos-input (inc (count params)))
             result (apply (fun op) params)]
         (case op
           4 (recur (+ ip 2) input)
           5 (recur (if (zero? (first params))
                      (+ ip 3)
                      (second params)) input)
           6 (recur (if (zero? (first params))
                      (second params)
                      (+ ip 3)) input)
           (recur (+ ip (+ 2 (count params))) (assoc input r* result))))))))

(run input)