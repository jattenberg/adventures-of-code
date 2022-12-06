(ns aoc22.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            ))

(def input (->> (io/resource "aoc22/day05.txt")
       (slurp)
       (str/split-lines)))

(defn conj*
  [s x]
  (conj (vec s) x))

(defn swap-cargo
  "takes the last exntry of from and puts it on top of to"
  [c from to]
  (let [fromc (get c from)
        last (last fromc)
        fromcn (drop-last fromc)
        toc (get c to)
        tocn (conj* toc last)
        with-tocn (assoc c to tocn)]
    (assoc with-tocn from fromcn)))

(defn one-instruction
  "swaps the cargo according to one instruction"
  [c, i]
  (let [ct (first i)
        from (second i)
        to (last i)
        swapped (swap-cargo c from to)]
    (if (> ct 1)
      (one-instruction swapped [(dec ct) from to])
      swapped)))

(defn transpose
  "takes a seq of seqs and transposes it"
  [x]
  (apply mapv vector x))

; "[F] " "[Z] " "[P] " "[C] " "[G] " "[D] " "[L] " "    "
(defn clean-cargo
  "takes a list like above, removes blanks and non-letter chars"
  [r]
  (->> (map #(str/replace % #"\W+" "") r)
       (keep not-empty)
       vec))

;  "[N]     [C]                 [Q]    "
;  "[W]     [J] [L]             [J] [V]"
;  "[F]     [N] [D]     [L]     [S] [W]"
;  "[R] [S] [F] [G]     [R]     [V] [Z]"
;  "[Z] [G] [Q] [C]     [W] [C] [F] [G]"
;  "[S] [Q] [V] [P] [S] [F] [D] [R] [S]"
;  "[M] [P] [R] [Z] [P] [D] [N] [N] [M]"
;  "[D] [W] [W] [F] [T] [H] [Z] [W] [R]"
;  " 1   2   3   4   5   6   7   8   9 "
; into:
;[("D" "M" "S" "Z" "R" "F" "W" "N")
; ("W" "P" "Q" "G" "S")
; ("W" "R" "V" "Q" "F" "N" "J" "C")
; ("F" "Z" "P" "C" "G" "D" "L")
; ("T" "P" "S")
; ("H" "D" "F" "W" "R" "L")
; ("Z" "N" "D" "C")
; ("W" "N" "R" "F" "V" "S" "J" "Q")
; ("R" "M" "S" "G" "Z" "W" "V")]
(defn parse-cargo
  "parses a map of the cargo that looks like the above comment"
  [lines]
  (->> (apply list lines) ; without thinking about cons
       (map #(re-seq #".{3}\s?" %))
       reverse
       (drop 1) ; i'm just going to use the column indices
       transpose
       (map clean-cargo)
       (vec)))

(defn dec-inst
  "decrements the indices of the instructions"
  [i]
  [(first i)
    (dec (second i))
    (dec (last i))])

;  "move 29 from 4 to 9"
;  "move 15 from 9 to 7"
;  "move 1 from 5 to 1"
;  "move 9 from 8 to 2"
;  "move 10 from 9 to 3"
(defn parse-inst
  "parses instructions that look like above into triples"
  [lines]
  (->> (apply list lines)
       (map #(re-seq #"\d+" %))
       (map #(map parse-long %))
       (map dec-inst)))

(defn part-1
  "Run with bb -x aoc22.day05/part-1"
  [_]
  (let [stanzas (partition-by str/blank? input)
        cargo (parse-cargo (first stanzas))
        instrs (parse-inst (last stanzas))]
    (->> (reduce one-instruction cargo instrs)
         (map first)
         (str/join)))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
  (->> 
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (sort-by -)
       (take 3)
       (apply +)
       prn))
