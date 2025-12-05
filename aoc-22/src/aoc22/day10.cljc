(ns aoc22.day10
  (:require [utils :as utils]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn multiplication-points
  ([] (multiplication-points 20))
  ([n] (lazy-seq (cons n (multiplication-points (+ 40 n))))))

(defn apply-commands
  [history commands]
  (if (= 0 (count commands))
    history
    (let [next-cmd (first commands)
          last-val (last history)
          next-reg (cond (= "noop" next-cmd) [last-val]
                         (str/includes? next-cmd "addx") [last-val (->> (str/split next-cmd #" ")
                                                                        (last)
                                                                        (parse-long)
                                                                        (#(+ % last-val)))])]
      (recur (concat history next-reg) (rest commands)))))

(defn find-beam
  [c]
  [(dec c) c (inc c)])

(defn drawn-pixels
  [i c]
  (if (utils/in? c i) i))

; if the sprite's horizontal position puts its pixels where the CRT is currently drawing, then those pixels will be drawn.
(defn row-vals-2-pixels
  [row]
  (->> (map find-beam row)
       (map-indexed (fn [i c] (if (utils/in? c i) i)))
       (keep identity)))

(defn add-cmds-to-row
  [row cmds]
  (if (= 0 (count cmds))
    row
    (let [nxt (first cmds)
          new-row (assoc row nxt "#")]
      (recur new-row (rest cmds)))))

(defn render-row
  [cmds]
  (let [sz 40
        row (->> (repeat sz ".")
                 (vec))]
    (add-cmds-to-row row cmds)))

(defn part-1
  "Run with bb -x aoc22.day10/part-1"
  [_]
  (let [input (->> (io/resource "aoc22/day10.txt")
                   (slurp)
                   (str/split-lines))
        initial 1
        register-vals (vec (apply-commands [initial] input))
        inspection-pts (vec (take 6 (multiplication-points)))

        values (map #(* % (nth register-vals (dec %))) inspection-pts)]
    (println (utils/zip inspection-pts values))
    (reduce + values)))

;####.###...##..###..####.###...##....##.
;#....#..#.#..#.#..#.#....#..#.#..#....#.
;###..#..#.#....#..#.###..#..#.#.......#.
;#....###..#....###..#....###..#.......#.
;#....#.#..#..#.#.#..#....#....#..#.#..#.
;####.#..#..##..#..#.####.#.....##...##..
(defn part-2
  "Run with bb -x aoc22.day10/part-2"
  [_]
  (let [input (->> (io/resource "aoc22/day10.txt")
                   (slurp)
                   (str/split-lines))
        initial 1
        register-vals (vec (apply-commands [initial] input))
        lines (partition 40 register-vals)
        pixel-cmds (mapv row-vals-2-pixels lines)
        pixel-rows (mapv render-row pixel-cmds)]
    (doseq [i pixel-rows] (println  (str/join "" i)))))
