#!/usr/bin/env bb
(ns day16
  (:require
    [clojure.string :as str]))
            
(def input-path (or (first *command-line-args*) "2021/inputs/day16.txt"))

(defn hex-char->binary-string [hex-char]
  (let [unpadded-binary-str (Integer/toBinaryString (Character/digit ^Character hex-char 16))]
    (str/replace (format "%4s" unpadded-binary-str) \space \0)))

(defn str->binary-seq
  {:test #(do
            (assert (= [\1 \1 \0 \1 \0 \0 \1 \0 \1 \1 \1 \1 \1 \1 \1 \0 \0 \0 \1 \0 \1 \0 \0 \0] (str->binary-seq "D2FE28")))
            (assert (= [\0 \0 \1 \1 \1 \0 \0 \0 \0 \0 \0 \0 \0 \0 \0 \0 \0 \1 \1 \0 \1 \1 \1 \1 \0 \1 \0 \0 \0 \1 \0 \1 \0 \0 \1 \0 \1 \0 \0 \1 \0 \0 \0 \1 \0 \0 \1 \0 \0 \0 \0 \0 \0 \0 \0 \0] (str->binary-seq "38006F45291200"))))}
  [input-str]
  (mapcat hex-char->binary-string input-str))

(defn conform-str [str-or-char-seq]
  (apply str str-or-char-seq))

(defn parse-binary [str-or-char-seq]
  (Long/parseLong (conform-str str-or-char-seq) 2))

(defn char-seq->long [chars]
  (Integer/parseInt (apply str chars) 2))

(defn parse-header [input-str]
  (let [header (take 6 input-str)
        [version type-id] (map char-seq->long (split-at 3 header))]
    {:version version
     :type-id type-id}))

(defmulti parse-packet (comp :type-id parse-header))

(def literal-packet-pattern #"([10]{3})([10]{3})((?:1[01]{4})*(?:0[01]{4}))")

;; literal
(defmethod parse-packet 4 [in]
  (let [header (parse-header in)
        in-str (apply str in)
        [match _version _type payload] (re-find literal-packet-pattern in-str)
        content (->> payload
                  (partition 5)
                  (mapcat rest)
                  (apply str))
        value (Long/parseLong content 2)]
    (merge
      header
      {:bit-length (count match)
       :version-sum (:version header)
       :clojure value})))

(declare parse-packets)
;; operator
(defmethod parse-packet :default [in]
  (let [header (parse-header in)
        payload (drop 6 in)
        mode (first payload)
        payload (drop 1 payload)
        {:keys [sub-packets] :as content-map}
        (if (= \0 mode)
          (let [bit-length (parse-binary (take 15 payload))
                payload (drop 15 payload)
                packets (parse-packets (conform-str (take bit-length payload)))]
            {:bit-length (+ 3 3 1 15 bit-length)
             :mode 0
             :version-sum (+ (:version header) (transduce (map :version-sum) + packets))
             :sub-packets packets})
          (let [no-of-packets (parse-binary (take 11 payload))
                payload (drop 11 payload)
                packets (parse-packets payload no-of-packets)
                length (+ 3 3 1 11 (transduce (map :bit-length) + packets))]
            {:sub-packets packets
             :mode 1
             :version-sum (+ (:version header) (transduce (map :version-sum) + packets))
             :bit-length length}))
        clojure-code
        (case (:type-id header)
          0 `(+ ~@(mapv :clojure sub-packets))
          1 `(* ~@(mapv :clojure sub-packets))
          2 `(min ~@(mapv :clojure sub-packets))
          3 `(max ~@(mapv :clojure sub-packets))
          5 `(if (> ~@(map :clojure sub-packets)) 1 0)
          6 `(if (< ~@(map :clojure sub-packets)) 1 0)
          7 `(if (= ~@(map :clojure sub-packets)) 1 0)
          "Not implemented!")]
    (merge header content-map
      {:clojure clojure-code})))

(defn only-zero? [s]
  (every? #{\0} s))

(defn parse-packets
  ([in] (parse-packets in ##Inf))
  ([in n]
   (loop [left n
          in (seq in)
          packets []]
     (if (or (zero? left) (only-zero? in))
       packets
       (let [next-packet (parse-packet in)]
         (recur (dec left) (drop (:bit-length next-packet) in) (conj packets next-packet)))))))

(defn BITS [str]
  (eval (-> str str->binary-seq parse-packet :clojure)))

;;; Task 1
(defn task1
  {:test #(do
            (assert (= 16 (task1 "8A004A801A8002F478")))
            (assert (= 12 (task1 "620080001611562C8802118E34")))
            (assert (= 23 (task1 "C0015000016115A2E0802F182340")))
            (assert (= 31 (task1 "A0016C880162017C3686B18A3D4780"))))}
  [input]
  (->> input
    str->binary-seq
    parse-packet
    :version-sum))

(def task2 BITS)

;;; Print output
;;; Print output
(println "Task 1:" (task1 (slurp input-path)))
(println "Task 2:" (BITS (slurp input-path)))