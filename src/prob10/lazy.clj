(ns prob10.lazy)

(def primes
  (remove
    (fn [x]
      (some #(zero? (mod x %)) (take (Math/sqrt x) primes)))
    (iterate inc 2)))

(defn solve [n]
  (apply + (take-while #(<= % n) primes)))