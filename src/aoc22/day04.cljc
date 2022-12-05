(ns aoc22.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as cset]
            ))

(defn string-to-set
  "takes a string like \\d+-\\d+, and turns it into a set of ints"
  [s]
  (let [strs (str/split s #"-")
        nums (map parse-long strs)
        rng (range (first nums) (+ 1 (last nums)))]
    (set rng)))

(defn lines-to-sets
  "prepping for doomsday"
  [l]
  (->> l
       ((fn [s] (str/split s #",")))
       (map string-to-set)
   ))

;; new line delim'd text, blank spaces denote different
;; elves
(def input (->> (slurp (io/resource "aoc22/day04.txt"))
                (str/split-lines)
                (map lines-to-sets)
                ))

(defn fully-contains?
  "input is two sets. determines if one set is a proper subset of the other"
  [a b]
  (or (cset/subset? a b) (cset/subset? b a)))


(defn part-1
  "Run with bb -x aoc22.day04/part-1"
  [_]
  (->> input
       (map #(apply fully-contains? %))
       (keep #(if (true? %) %)) ; keep those that are true
       count))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
  (->> input
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (sort-by -)
       (take 3)
       (apply +)
       prn))
