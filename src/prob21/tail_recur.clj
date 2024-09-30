(ns prob21.tail-recur)

(defn sum-of-divisors [n]
  (loop [i 1 acc 0]
    (if (>= i n)
      acc
      (if (zero? (mod n i))
        (recur (inc i) (+ acc i))
        (recur (inc i) acc)))))

(defn sum-amicable-numbers [limit]
  (loop [n 2 acc 0]
    (if (>= n limit)
      acc
      (let [pair-sum (sum-of-divisors n)]   ;; Сумма делителей n
        (if (and (not= n pair-sum)          ;; Если n не равно pair-sum
                 (= n (sum-of-divisors pair-sum)))  ;; И если сумма делителей pair-sum равна n
          (recur (inc n) (+ acc n))
          (recur (inc n) acc))))))

(defn solve [n]
  (sum-amicable-numbers n))
