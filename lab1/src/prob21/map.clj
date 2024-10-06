(ns prob21.map)
(defn sum-divisors [n]
  (let [divisors (for [i (range 1 (inc (Math/sqrt n)))
                       :when (zero? (mod n i))]
                   (if (= i (quot n i))
                     i
                     (list i (quot n i))))]
    (->> divisors
         flatten
         (remove #{n})
         (distinct)
         (apply +))))

(defn friendly-numbers [limit]
  (let [d (into {} (map (fn [n] [n (sum-divisors n)]) (range 1 limit)))]
    (->> d
         (filter (fn [[a b]] (and (not= a b) (= (get d b) a))))
         (map first)
         (distinct)
         (apply +))))

(defn solve [n]
  (friendly-numbers n))
