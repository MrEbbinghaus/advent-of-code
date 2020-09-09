(ns day02.day02
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (map edn/read-string (-> "src/day02/input.txt" slurp (str/split #","))))

(defn run
  ([input] (run 0 (vec input)))
  ([position input]
   (let [[op p1 p2 r] (drop position input)
         p1v (get input p1)
         p2v (get input p2)
         result (case op
                  1 (+ p1v p2v)
                  2 (* p1v p2v)
                  :whatever)]
     (if (= op 99)
       (first input)
       (recur (+ position 4) (assoc input r result))))))

(defn with-params [program p1 p2]
  (assoc (vec program) 1 p1 2 p2))

(comment
  "Task 1"
  (run (with-params input 12 2))

  "Task 2 - Fast enough"#"¯\_(ツ)_/¯"
  (get (into {}
         (for [p1 (range 100)
               p2 (range 100)]
           [(run (with-params input p1 p2)) [p1 p2]]))
    19690720))
