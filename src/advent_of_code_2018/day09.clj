(ns advent-of-code-2018.day09)

#_(def input "468 players; last marble is worth 71010 points")
(def input {:player 468 :marbles 71010})

(defn rr-current [marbles current]
  (inc (mod (+ current 1) (count marbles))))

(defn l7-current [marbles current]
  (mod (- current 7) (count marbles)))

(defn add-marble [marbles current marble]
  (let [[front back] (split-at current marbles)]
    (lazy-cat front [marble] back)))

(defn remove-marble [marbles i]
  (let [front (subvec (vec marbles) 0 i)
        back (subvec (vec marbles) (inc i))]
    (concat front back)))

(time (remove-marble [1 2 3] 1))



(defn play [players max-marble]
  (loop [player 0
         marbles [0]
         current 0
         marble 1
         scores (zipmap (range players) (repeat 0))]
    (when (zero? (mod marble 1000))
      (println "Marble No:" marble))
    (if (> marble max-marble)
      scores
      (if (zero? (mod marble 23))
        (let [new-current (l7-current marbles current)]
          (recur
            (mod (inc player) players)
            (remove-marble marbles new-current)
            new-current
            (inc marble)
            (update scores player #(+ % marble (nth marbles new-current)))))


        (let [new-current (rr-current marbles current)]
          (recur
            (mod (inc player) players)
            (add-marble marbles new-current marble)
            new-current
            (inc marble)
            scores))))))

(rr-current [0] 0)
(rr-current [0 2 1] 1)
(add-marble [0 2 1] (rr-current [0 2 1] 1) 3)
(rr-current '(0 2 1 3) 3)
(add-marble '(0 2 1 3) (rr-current '(0 2 1 3) 3) 4)

; solution1 prepare for 15min of waiting
(time (apply max-key second (map vec (play 468 71010))))