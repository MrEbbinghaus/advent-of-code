

(defn part1 [x]
    (let [a (dec (* 2 (Math/ceil (/ (inc (Math/sqrt x)) 2))))]
      (- (* 2 (dec (Math/ceil (/ a 2)))) (mod (- (* a a) x) (dec a)))))


