(ns aoc22.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as cset]))

(defn string-to-set
  "takes a string like \\d+-\\d+, and turns it into a set of ints"
  [s]
  (let [strs (str/split s #"-")
        nums (map parse-long strs)
        rng (range (first nums) (+ 1 (last nums)))]
    (set rng)))

(defn lines-to-sets
  [l]
  (->> (str/split l #",")
       (map string-to-set)))

;; new line delim'd text, blank spaces denote different
;; elves
(def input (->> (slurp (io/resource "aoc22/day04.txt"))
                (str/split-lines)
                (map lines-to-sets)))

(defn fully-contains?
  "input is two sets. determines if one set is a proper subset of the other"
  [a b]
  (or (cset/subset? a b) (cset/subset? b a)))

(defn partially-contains?
  "input is two sets, determines if one set intersects with the other"
  [a b]
  (let [i (cset/intersection a b)
        c (count i)]
    (if (> c 0) c)))

(defn part-1
  "Run with bb -x aoc22.day04/part-1"
  [_]
  (->> input
       (map #(apply fully-contains? %))
       (keep #(if (true? %) %))
       count))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
  (->> input
       (map #(apply partially-contains? %))
       (keep identity)
       count))
