(ns day08.day08
  (:require [clojure.edn :as edn]))

(def input (->> "src/day08/input.txt" slurp butlast (map (comp edn/read-string str))))

(def width 25)
(def height 6)

(defn layers [input]
  (partition (* width height) input))

(defn merge-pixels [& pixels]
  (first (drop-while #{2} pixels)))

(defn print-layer [layer]
  (dotimes [h height]
    (dotimes [w width]
      (print (case (get (vec layer) (+ (* h width) w))
               0 " "
               1 "X") " "))
    (println)))

(comment
  "Task 1"
  (let [counted-layers (into {} (map #(vector (count (filter zero? %)) %)) (layers input))
        freqs (frequencies (counted-layers (apply min (keys counted-layers))))]
    (* (freqs 1) (freqs 2)))


  (print-layer (apply map merge-pixels (layers input))))




