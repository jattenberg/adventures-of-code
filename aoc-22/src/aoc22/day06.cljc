(ns aoc22.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn find-keys
  [idx s]
  (let [dist (distinct s)
        in-ct (count s)
        dist-ct (count dist)]
    (if (= in-ct dist-ct) idx)))

(defn part-1
  "Run with bb -x aoc22.day06/part-1"
  [_]
  (let [sz 4
        input (str/trim (slurp (io/resource "aoc22/day06.txt")))
        windows (partition sz 1 input)]
    (->> (map-indexed find-keys windows)
         (keep identity)
         (first)
         (+ sz))))

(defn part-2
  "Run with bb -x aoc22.day06/part-1"
  [_]

  (let [sz 14
        input (str/trim (slurp (io/resource "aoc22/day06.txt")))
        windows (partition sz 1 input)]
    (->> (map-indexed find-keys windows)
         (keep identity)
         (first)
         (+ sz))))

