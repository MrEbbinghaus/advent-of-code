(def my-input 347991)

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

;;
;;
;; The second task.
;;

(def left 0)
(def up 1)
(def right 2)
(def down 3)
(def directions [[-1 0]   ; left
                 [0 1]    ; up
                 [1 0]    ; right
                 [0 -1]]) ; down

(defn next-position
    [position direction]
    (map + position (get directions direction)))

(defn next-direction [spiral position direction]
    "Returns the directio to go to. A new one, if to the right (in view direction) is nothing, else the old one."
    (let [possible-direction (mod (inc direction) 4)]
      (if (contains? spiral (next-position position possible-direction))
        direction
        possible-direction)))

(defn sum-adjacent
  "Return the sum of all adjacent coordinates to the given [x y] coordinate."
  [spiral [x y]]
  (reduce + 0
    (filter identity (map #(get spiral %)
                       (for [xx (range (dec x) (+ x 2))
                             yy (range (dec y) (+ y 2))
                             :when (or (not= x xx) (not= y yy))]
                         [xx yy])))))

(defn task2 [input]
    (loop [spiral {[0 0] 1}
           last [0 0]
           direction right]
      (let [next-pos (next-position last direction)
            sum (sum-adjacent spiral next-pos)]
        (if (< input sum)
          sum
          (recur
            (assoc spiral next-pos sum)
            next-pos
            (next-direction spiral next-pos direction))))))

(task2 my-input)




