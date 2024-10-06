(ns prob21.loop)

(defn sum-of-divisors [n]
  (loop [i 1 acc 0]
    (if (>= i n)
      acc
      (if (zero? (mod n i))
        (recur (inc i) (+ acc i))
        (recur (inc i) acc)))))

(defn sum-friendly-numbers [limit]
  (loop [n 2 acc 0]
    (if (>= n limit)
      acc
      (let [pair-sum (sum-of-divisors n)]
        (if (and (not= n pair-sum) (= n (sum-of-divisors pair-sum)))
          (recur (inc n) (+ acc n))
          (recur (inc n) acc))))))

(defn solve [n]
  (sum-friendly-numbers n))
