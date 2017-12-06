(def my-input [0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11])

(defn index-of-biggest [col]
    (.indexOf col (first (sort > col))))

(defn distribute [col start]
    (let [size (count col)
          value (nth col start)]
      (apply map + (assoc (vec col) start 0)
        (partition size size (repeat (- size (dec start)) 0)
          (concat (repeat (inc start) 0) (repeat value 1))))))

(defn task1 [input]
    (loop [old-ones #{}
           banks input
           steps 0]
      (if (contains? old-ones banks)
        steps
        (recur (conj old-ones banks)
               (distribute banks (index-of-biggest banks))
               (inc steps)))))

(defn task2 [input]
    (loop [old-ones {}
           banks input
           steps 0]
      (if (contains? old-ones banks)
        (- steps (get old-ones banks))
        (recur (assoc old-ones banks steps)
               (distribute banks (index-of-biggest banks))
               (inc steps)))))

(task1 my-input)
(task2 my-input)


