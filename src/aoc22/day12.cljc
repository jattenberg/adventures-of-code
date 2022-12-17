(ns aoc22.day12
  (:require [clojure.java.io :as io]
            [utils :as utils]
            [clojure.string :as str]))

(def letter-ind (->> (map-indexed
                      (fn [a b] [b a])
                      (clojure.string/split "abcdefghijklmnopqrstuvwxyz" #""))
                     (into {})))

;; new line delim'd text, blank spaces denote different
;; elves
(def input (->> (slurp (io/resource "aoc22/day12.txt"))
                (str/split-lines)
                (map parse-long)))

(defn letter-dist
  [a b]
  (let [a-pos (get letter-ind a)
        b-pos (get letter-ind b)]
    (abs (- a-pos b-pos))))

(defn visit-node
  [map [row col]]
  (let [this-r (get-in map [row col]) ; todo: do somethign when it's E or S
        this (cond (= this-r "S") "a"
                   (= this-r "E") "z"
                   :else this-r)
        above [(dec row) col]
        a-val (get-in map above)
        below [(inc row) col]
        b-val (get-in map below)
        left [row (dec col)]
        l-val (get-in map left)
        right [row (inc col)]
        r-val (get-in map right)
        coords [above below left right]
        heights [a-val b-val l-val r-val]]
    (->> (utils/zip coords heights)
         (filter #(and
                   (not (nil? (second %)))
                   (<= (letter-dist this (second %)) 1)))))) ; only keep reachable neighbors

(defn step-fn
  [map paths seen steps start end end-v])

(defn dfs
  ([map start end] (dfs map start end [] (set []) []))
  ([map start end paths seen steps]
   (let [neighbors (visit-node map start)
         _ (println neighbors)
         new-seen (conj seen start)
         _ (println new-seen)
         seen-fn (fn [l] (not (contains? new-seen (first l))))
         unvisited (filter seen-fn neighbors)
         _ (println unvisited)]
     (if (= 0 (count unvisited))
       paths ; no more steps to explore, todo also return seen
       (map (fn [[r c] v]
              (if (= v "E")
                (conj paths (conj steps [[r c] v]))
                (dfs [r c] end paths new-seen (conj steps [[r c] v]))) unvisited))))))

(defn find-path
  ([map start end] (find-path map start end dfs))
  ([map start end search-fn]
   (search-fn map start end)))

(defn path-length
  [path])

(defn part-1
  "Run with bb -x aoc22.day12/part-1"
  [_]
  (let [map (->> (io/resource "aoc22/day12.txt")
                 slurp
                 (str/split-lines)
                 (map #(str/split % #""))
                 (to-array-2d))
        s-pos (first (utils/find-match-in-2d-array map #(= % "S")))
        e-pos (first (utils/find-match-in-2d-array map #(= % "E")))
        path (find-path map s-pos e-pos)]
    (path-length path)))

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
