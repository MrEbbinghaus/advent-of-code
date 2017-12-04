

(defn part1 [x]
    (let [a (dec (* 2 (Math/ceil (/ (inc (Math/sqrt x)) 2))))]
;;        | The root of the next greater square with odd root -> aka the root of the last number in a shell.
      (- (* 2 (dec (Math/ceil (/ a 2)))) (mod (- (* a a) x) (dec a)))))
;;            | The number of the "shell" -> [1] = 0, [2-9] = 1, [10-25] = 2, ...
;;       | The max distance in this "shell" 1 = 0, 9 = 2, 25 = 4, ... (all squares with odd root are corners) 
;;                                       |    |             | size of an edge of the shell - 1,
;;                                       |    |             | because you don't want to count the corners twice
;;                                       |    | The distance from the last field in a shell
;;                                       | distance from the next corner of a shell


