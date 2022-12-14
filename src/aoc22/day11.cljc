(ns aoc22.day11
  (:require [clojure.java.io :as io]
            [utils :as utils]
            [clojure.string :as str]))

(defn parse-monkeys
  [parts]
  (let [m-id (->> (first parts)
                  (re-matches #"Monkey\s+(\d+):")
                  (last)
                  (parse-long))
        items (->> (second parts)
                   (re-matches #"Starting items: (.*)")
                   (last)
                   (#(str/split % #",\s+"))
                   (map parse-long))
        oper (->> (nth parts 2)
                  (re-matches #"Operation: new = old (\S)\s+(\S+)")
                  (take-last 2))
        test (->> (nth parts 3)
                  (re-matches #"Test:\s+divisible\s+by\s+(\d+)")
                  (last)
                  (parse-long))
        if-true (->> (nth parts 4)
                     (re-matches #"If\s+true:\s+throw\s+to\s+monkey\s+(\d+)")
                     (last)
                     (parse-long))
        if-false (->> (nth parts 5)
                      (re-matches #"If\s+false:\s+throw\s+to\s+monkey\s+(\d+)")
                      (last)
                      (parse-long))]
    {:m-id m-id
     :items items
     :oper oper
     :test test
     :if-true if-true
     :if-false if-false}))

(defn part-1
  "Run with bb -x aoc22.day11/part-1"
  [_]
  (let [input (->> (io/resource "aoc22/day11.txt")
                   (slurp)
                   (str/split-lines)
                   (map str/trim)
                   (partition-by str/blank?)
                   (take-nth 2))
        monkeys (map parse-monkeys input)]
    input))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_])
