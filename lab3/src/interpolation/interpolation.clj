(ns interpolation.interpolation
  (:require [clojure.string :as str]))

(defn parse-point [input]
  (let [[x y] (str/split input #"[ \t,;]+")]
    [(Double/parseDouble x) (Double/parseDouble y)]))

(defn interpolate-linear [data step-size]
  (let [[point1 point2] data
        [x1 y1] point1
        [x2 y2] point2
        x-range (range x1 (+ x2 step-size) step-size)]
    (map (fn [x]
           (let [factor (/ (- x x1) (- x2 x1))]
             [(double x) (+ y1 (* factor (- y2 y1)))]))
         x-range)))

(defn lagrange [data x]
  (let [n (count data)]
    (reduce
      (fn [accum i]
        (let [[xi yi] (nth data i)
              term (reduce (fn [prod j]
                             (if (not= i j)
                               (let [[xj _] (nth data j)]
                                 (* prod (/ (- x xj) (- xi xj))))
                               prod))
                           1
                           (range n))]
          (+ accum (* term yi))))
      0
      (range n))))

(defn interpolate-lagrange [data step start end]
  (let [x-range (range start (+ end step) step)]
    (map #(vector % (lagrange data %)) x-range)))

(defn format-results [x-values y-values]
  (let [x-line (str/join "\t" (map #(format "%.2f" %) x-values))
        y-line (str/join "\t" (map #(format "%.2f" %) y-values))]
    (str x-line "\n" y-line "\n")))