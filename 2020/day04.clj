#!/usr/bin/env bb
(ns day04
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

(defn split-empty-lines [s]
  (str/split s #"\n\n"))

(defn split-by-whitespace [s]
  (str/split s #"\s"))

(defn split-field [s]
  (str/split s #":"))

(defn parse-passport [str-passport]
  (into {} (map split-field) str-passport))

(def *input* (->> *in* slurp split-empty-lines (map split-by-whitespace) (map parse-passport)))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn all-fields-present? [passport]
  (set/subset? required-fields (set (keys passport))))

(defmulti valid-field? first)
(defmethod valid-field? "byr" [[_ v]] (<= 1920 (Integer/parseInt v)  2002))
(defmethod valid-field? "iyr" [[_ v]] (<= 2010 (Integer/parseInt v)  2020))
(defmethod valid-field? "eyr" [[_ v]] (<= 2020 (Integer/parseInt v)  2030))
(defmethod valid-field? "hgt" [[_ v]]
  (if-let [[_ height-str unit] (re-find #"^(\d{2,3})(in|cm)$" v)]
    (let [height (Integer/parseInt height-str)]
      (case unit
        "cm" (<= 150 height 193)
        "in" (<= 59 height 76)
        false))
    false))
(defmethod valid-field? "hcl" [[_ v]] (re-matches #"#[0-9a-f]{6}" v))
(defmethod valid-field? "ecl" [[_ v]] (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v))
(defmethod valid-field? "pid" [[_ v]] (re-matches #"\d{9}" v))
(defmethod valid-field? "cid" [_] true)

(defn solve-task1 [input]
  (->> input (filter all-fields-present?) count))

(defn all-fields-valid? [passport]
  (every? valid-field? passport))

(defn solve-task2 [input]
  (->> input (filter all-fields-present?) (filter all-fields-valid?) count))

(println "Answer for star 1:" (solve-task1 *input*))
(println "Answer for star 2:" (solve-task2 *input*))