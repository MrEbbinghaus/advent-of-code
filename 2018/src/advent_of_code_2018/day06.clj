(ns advent-of-code-2018.day06)

(def input [[342, 203] [79, 64] [268, 323] [239, 131] [246, 87] [161, 93] [306, 146] [43, 146] [57, 112] [241, 277] [304, 303] [143, 235] [253, 318] [97, 103] [200, 250] [67, 207] [345, 149] [133, 222] [232, 123] [156, 359] [80, 224] [51, 145] [138, 312] [339, 294] [297, 256] [163, 311] [241, 321] [126, 66] [145, 171] [359, 184] [241, 58] [108, 312] [117, 118] [101, 180] [58, 290] [324, 42] [141, 190] [270, 149] [209, 294] [296, 345] [68, 266] [233, 281] [305, 183] [245, 230] [161, 295] [335, 352] [93, 66] [227, 59] [264, 249] [116, 173]])

(defn distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^long (- x1 x2))
     (Math/abs ^long (- y1 y2))))

(defn md-convex-hull [ps]
  (let [left   (first ps)
        right  (last ps)
        bottom (map first ps)
        top    (map last ps)]
    (set (concat top bottom left right))))

(defn solution1 [input]
  (let [left-boundry  (apply min (map first input))
        right-boundry (apply max (map first input))
        upper-boundry (apply min (map second input))
        lower-boundry (apply max (map second input))
        nearest       (for [x (range left-boundry right-boundry)
                            y (range upper-boundry lower-boundry)]
                        (let [distances (sort (map (partial distance [x y]) input))
                              tie?      (= (first distances) (second distances))]
                          (if tie? :tie
                                         (apply min-key (partial distance [x y]) input))))
        hull          (md-convex-hull (partition (inc (- right-boundry left-boundry)) nearest))]
    (->> nearest
      (remove #{:tie})
      (remove hull)
      frequencies
      vals
      (apply max))))

(time (solution1 input))


(defn solution2 [input]
  (let [left-boundry  (apply min (map first input))
        right-boundry (apply max (map first input))
        upper-boundry (apply min (map second input))
        lower-boundry (apply max (map second input))
        positions     (for [x (range left-boundry right-boundry)
                            y (range upper-boundry lower-boundry)]
                        [x y])]
    (count (filter #(> 10000 (reduce + (map (partial distance %) input))) positions))))

(time (solution2 input))
