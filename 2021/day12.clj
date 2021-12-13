#!/usr/bin/env bb
(ns day12
  (:require [clojure.string :as str]
            [clojure.set :as set]))
            
(def input-path (or (first *command-line-args*) "2021/inputs/day12.txt"))

(defn bidirectional-map [[a b]]
  (cond
    (= :start a) {a #{b}}
    (= :start b) {b #{a}}
    :else
    {a #{b}
     b #{a}}))

(defn parse-input [input]
  (->> (str/split input #"[\n\-]")
    (map keyword)
    (partition 2)
    (map bidirectional-map)
    (apply merge-with set/union)))

(def input (parse-input (slurp input-path)))
(def test-input-small (parse-input "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"))
(def test-input (parse-input "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"))
(def test-input-large (parse-input "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"))

(defn small-cave? [k]
  (<= (int \a) (int (first (name k)))))

;;; Task 1
(defn prefix-paths
  ([cave-system]
   (prefix-paths cave-system contains? {} :start))

  ([cave-system blocked?]
   (prefix-paths cave-system blocked? {} :start))

  ([cave-system blocked? seen node]
   (let [new-seen
         (if (small-cave? node)
           (update seen node (fnil inc 0))
           seen)]
     {node
      (if (= node :end)
        :$
        (into {}
          (comp
            (remove #(blocked? new-seen %))
            (keep #(prefix-paths cave-system blocked? new-seen %)))
          (get cave-system node)))})))


(defn no-of-paths
  ([cave-system]
   (no-of-paths cave-system contains? {} :start))

  ([cave-system blocked?]
   (no-of-paths cave-system blocked? {} :start))

  ([cave-system blocked? seen node]
   (letfn
     [(no-of-paths' [seen node]
        (let [new-seen
              (if (small-cave? node)
                (update seen node (fnil inc 0))
                seen)]
          (if (= node :end)
            1
            (transduce
              (comp
                (remove #(blocked? new-seen %))
                (keep #(no-of-paths' new-seen %)))
              +
              (get cave-system node)))))]
     (no-of-paths' seen node))))


(defn nodes [tree]
  (tree-seq map? vals tree))


(defn task1
  {:test #(do
            (assert (= 10 (task1 test-input-small)))
            (assert (= 19 (task1 test-input)))
            (assert (= 226 (task1 test-input-large))))}
  [input]
  (no-of-paths input))

;;; Task 2
(defn task2
  {:test #(do
            (assert (= 36 (task2 test-input-small)))
            (assert (= 103 (task2 test-input)))
            (assert (= 3509 (task2 test-input-large))))}
  [input]
  (no-of-paths input
    (fn [seen node]
      (or
        (= :start node)
        (< 1 (seen node 0))
        (and
          (< 0 (seen node 0))
          (some #{2} (vals seen)))))))


;;; Print output
;;; Print output
(println "Task 1:" (time (task1 input)))
(println "Task 2:" (time (task2 input)))