(ns prob10.modular)

(defn prime? [n]
  (and (> n 1)
       (not-any? #(zero? (mod n %)) (range 2 (inc (Math/sqrt n))))))

(def primes
  (filter prime?
          (iterate inc 2)))

(defn solve [n]
  (apply + (cons 2 (take-while #(< % n) primes))))

