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
  (let [get-fn (fn [l] (get letter-ind
                            (cond (= l "S") "a"
                                  (= l "E") "z"
                                  :else l)))
        a-pos (get-fn a)
        b-pos (get-fn b)]
    (abs (- a-pos b-pos))))

(defn visit-node
  [pmap [row col]]
                                        ;(println (str "r " row " c " col))
  (let [this (get-in pmap [row col]) ; todo: do somethign when it's E or S
        above [(dec row) col]
        a-val (get-in pmap above)
        below [(inc row) col]
        b-val (get-in pmap below)
        left [row (dec col)]
        l-val (get-in pmap left)
        right [row (inc col)]
        r-val (get-in pmap right)
        coords [above below left right]
        heights [a-val b-val l-val r-val]]
    (->> (utils/zip coords heights)
         (filter #(and
                   (not (nil? (second %)))
                   (<= (letter-dist this (second %)) 1)))))) ; only keep reachable neighbors

(defn step-fn
  [map paths seen steps start end end-v])

(defn dfs
  ([pmap start end] (dfs pmap start end [] (set []) []))
  ([pmap start end paths seen steps]
   (let [neighbors (visit-node pmap start)
         new-seen (conj seen start)
         seen-fn (fn [l] (not (contains? new-seen (first l))))
         unvisited (filter seen-fn neighbors)
         options (count unvisited)
         step-fn (fn [l]
                   (let [[r c] (first l)
                         v (second l)
                         next-steps (conj steps l)]
                     (if (= v "E")
                       (conj paths next-steps)
                       (dfs pmap [r c] end paths new-seen next-steps))))]
     (println (str unvisited (step-fn (first unvisited))))
     (if (> options 0)
       (map step-fn unvisited)
       []))))

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
