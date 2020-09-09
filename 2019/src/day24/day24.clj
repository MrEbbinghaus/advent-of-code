(ns day24.day24
  (:require
    [clojure.string :as str]))

(def my-input (slurp "2019/src/day24/input.txt"))

(def symbol-mapping
  {\# :bug
   \. nil})

(defn parse-input [input]
  (let [rows (str/split-lines input)]
    {:gen 0
     :row-length (count (first rows))
     :board (map symbol-mapping (apply concat rows))}))

(defn pprint-world! [{:keys [board row-length]}]
  (let [printable-board (replace {:bug "🐞" nil "⬜️"} board)
        rows (partition row-length printable-board)]
    (doseq [row rows]
      (println (interpose \tab row)))))

(defn- neighbour-poss [pos row-length]
  ;; These checks are here to prevent wrapping
  [(if (zero? (mod (inc pos) row-length)) -1 (inc pos)) ; right
   (if (zero? (mod pos row-length)) -1 (dec pos)) ; left
   (+ pos row-length) ; down
   (- pos row-length)]) ; up

(defn alive? [board pos]
  (= (nth board pos nil) :bug))

(defn tick-position [{:keys [board row-length]} pos]
  (let [no-of-living-neighbors (count (filter #(alive? board %) (neighbour-poss pos row-length)))]
    (if (alive? board pos)
      (when (= 1 no-of-living-neighbors) :bug)
      (when (#{1 2} no-of-living-neighbors) :bug))))

(defn tick [{:keys [board] :as world}]
  (let [board-size (count board)]
    (-> world
      (update :gen inc)
      (assoc :board (mapv #(tick-position world %) (range board-size))))))

(defn biodiversity-rating [{:keys [board]}]
  (->> board
    (map-indexed
      (fn [i field]
        (if (= field :bug)
          (int (Math/pow 2 i))
          0)))
    (reduce +)))


(defn solve-part1 [input]
  (biodiversity-rating
    (loop [world (parse-input input)
           seen-boards #{}]
      (let [{:keys [board gen] :as new-world} (tick world)]
        (if (seen-boards board)
          (do
            (println (format "First duplicate occurs after %d min" gen))
            (pprint-world! new-world)
            new-world)
          (recur new-world (conj seen-boards board)))))))

(solve-part1 (slurp "2019/src/day24/input.txt"))