#!/usr/bin/env bb
(ns day06
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

(defn split-empty-lines [s]
  (str/split s #"\n\n"))

(defn parse-group [group]
  (map set (str/split-lines group)))

(def *input* (->> *in* slurp split-empty-lines (map parse-group)))

(defn no-of-anyone [group]
  (count (apply set/union group)))

(defn no-of-everyone [group]
  (count (apply set/intersection group)))

(defn solve-task1 [input]
  (reduce + (map no-of-anyone input)))

(defn solve-task2 [input]
  (reduce + (map no-of-everyone input)))

(println "Answer for star 1: " (solve-task1 *input*))
(println "Answer for star 2: " (solve-task2 *input*))