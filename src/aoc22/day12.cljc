(ns aoc22.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; new line delim'd text, blank spaces denote different
;; elves
(def input (->> (slurp (io/resource "aoc22/day12.txt"))
                (str/split-lines)
                (map parse-long)))

(defn part-1
  "Run with bb -x aoc22.day12/part-1"
  [_]
  (->> input
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (apply max)
       prn))

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
