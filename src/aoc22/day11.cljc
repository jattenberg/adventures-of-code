(ns aoc22.day11
  (:require [clojure.java.io :as io]
            [utils :as utils]
            [clojure.core.match :as match]
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
                   (map parse-long)
                   (vec))
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
     :if-false if-false
     :seen 0}))

(defn play-monkey
  [monkeys monkey]
  (let [from (:m-id monkey)
        if-true (:if-true monkey)
        if-false (:if-false monkey)
        [a b] (:oper monkey)
        items (:items monkey)
        prod (:prod monkey)
        ; compute new worry leves for everything (then divide by 3)
        update-fn (match/match [a b]
                    ["*" "old"] #(* % %)
                    ["*" x] #(* (parse-long x) %)
                    ["+" y] #(+ (parse-long y) %))
        ; test to apply is always some mod
        test-fn #(= 0 (mod % (:test monkey)))
        pass-item-fn (fn [ms is]
                       (if (= (count is) 0)
                         ms
                         (let [i (first is)
                               old-items (->> (rest is) vec)
                               _ (println (str "Monkey" from))
                               new-score (update-fn i) ; compute new worry level
                               _ (println (str "Worry level is " a " by " b " to " new-score))
                               with-boredem (mod new-score prod) ; (quot new-score 3) ; divide by 3
                               _ (println (str "Monkey gets bored with item. Worry level is divided by 3 to " with-boredem))
                               test (test-fn with-boredem) ; does this meet the test
                               _ (println (str "Current worry level " (if test "is" "is not") " divisible by " (:test monkey)))
                               to (if test if-true if-false) ; based on the test, where?
                               _ (println (str "Item with worry level " with-boredem " is thrown to monkey " to))
                               monkey-items (get-in ms [to :items]) ; get that monkeys items
                               old-count (get-in ms [from :seen])
                               new-items (->> (conj monkey-items with-boredem) vec)  ; add item
                               updated-ms (->> (assoc-in ms [to :items] new-items)
                                               vec
                                               (#(assoc-in % [from :items] old-items))
                                               vec
                                               (#(assoc-in % [from :seen] (inc old-count))))] ; give the monkey items
                           (recur  updated-ms old-items))))]
    (pass-item-fn monkeys items)))

(defn monkey-multiply
  [monkeys]
  (let [prod (->> (map :test monkeys)
                  (reduce *))]
    (map #(assoc % :prod prod) monkeys)))

(defn print-and-return
  [monkeys]
  (println "monkeys are holding items with worry levels:")
  (doseq [i (->> (sort-by :seen monkeys) reverse)] (println (str "Monkey " (:m-id i) ": " (:items i) " seen: " (:seen i))))
  monkeys)

(defn play-all-monkeys
  ([monkeys] (play-all-monkeys monkeys (range (count monkeys))))
  ([monkeys ids]
   (if (= (count ids) 0)
     (print-and-return monkeys)
     (let [id (first ids)
           hero (nth monkeys id)
           play (play-monkey monkeys hero)]
       (recur play (rest ids))))))

(defn play-n-times
  [monkeys n]
  (println (str "round " n))
  (if (= n 0)
    monkeys
    (recur (play-all-monkeys monkeys) (dec n))))

(defn part-1
  "Run with bb -x aoc22.day11/part-1"
  [_]
  (let [input (->> (io/resource "aoc22/day11.txt")
                   (slurp)
                   (str/split-lines)
                   (map str/trim)
                   (partition-by str/blank?)
                   (take-nth 2))
        monkeys (->> (map parse-monkeys input) vec)
        ct 20
        rounds (play-n-times monkeys ct)]
    (->> (map :seen rounds) sort reverse (take 2) (reduce *))))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
    (let [input (->> (io/resource "aoc22/day11.txt")
                   (slurp)
                   (str/split-lines)
                   (map str/trim)
                   (partition-by str/blank?)
                   (take-nth 2))
          monkeys (->> (map parse-monkeys input)
                       (monkey-multiply)
                       vec)
          ct 10000
          rounds (play-n-times monkeys ct)]
      ; total monkey business
      (->> (map :seen rounds) sort reverse (take 2) (reduce *))))
