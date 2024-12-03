(ns interpolation.interpolation
  (:require [clojure.string :as str]))

(defn parse-point [input]
  (let [[x y] (str/split input #"[ \t,;]+")]
    [(Double/parseDouble x) (Double/parseDouble y)]))

(defn interpolate-linear [[ [x1 y1] [x2 y2] ] step-size]
  (map (fn [x]
         [(double x) (+ y1 (* (/ (- x x1) (- x2 x1)) (- y2 y1)))])
       (range x1 (+ x2 step-size) step-size)))

(defn lagrange [data x]
  (reduce
    (fn [accum [xi yi]]
      (let [term (reduce
                   (fn [prod [xj _]]
                     (if (not= xi xj)
                       (* prod (/ (- x xj) (- xi xj)))
                       prod))
                   1
                   data)]
        (+ accum (* term yi))))
    0
    data))

(defn interpolate-lagrange [data step start end]
  (for [x (range start (+ end step) step)]
    [x (lagrange data x)]))

(defn format-results [x-values y-values]
  (let [x-line (str/join "\t" (map #(format "%.2f" %) x-values))
        y-line (str/join "\t" (map #(format "%.2f" %) y-values))]
    (str x-line "\n" y-line "\n")))