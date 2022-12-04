(ns aoc22.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as cjs]))

(def fl_val (- (int (.charAt "a" 0)) 1))

(defn cat-w-upper
  "takes the input, concats with upper cased version"
  [s]
  (str s (str/upper-case s)))

(def letters
  (->> "abcdefghijklmnopqrstuvwxyz"
       cat-w-upper
       (char-array)
       (map-indexed (fn [i x] [(str x) (+ i 1)]))
       (into (hash-map))))

(def input (->> (slurp (io/resource "aoc22/day03.txt"))
                   (str/split-lines)))

(defn item-appearing-twice
  "takes the contents of a rucksack, finds the item that appears twice"
  [r]
  (let
      [chars (char-array r)
       len (count chars)
       halves (split-at (/ len 2) chars)]
    (str (some (set (first halves)) (last halves)))))

(defn overlapping-item
  "takes a group of 3 bags, finds the item that overlaps"
  [x]
  (->>
   x
   (map char-array)
   (map set)
   (apply cjs/intersection)
   first
   str
   ))


(defn part-1
  "Run with bb -x aoc22.day03/part-1"
  [_]
  (->> input
       (map item-appearing-twice)
       (map letters)
       (reduce +)
       prn))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
  (->> input
       (partition 3)
       (map overlapping-item)
       (map letters)
       (reduce +)
       prn))
