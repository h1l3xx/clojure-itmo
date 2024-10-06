(ns prob10.tail)

(defn prime? [n]
  (and (> n 1)
       (not-any? #(zero? (mod n %)) (range 2 (inc (Math/sqrt n))))))

(defn func [nums value]
  (if (zero? (mod nums 2))
    (recur (- nums 1) value)
    (if (<= nums 1)
      value
      (if (prime? nums)
        (recur (- nums 2) (+ value nums))
        (recur (- nums 2) value)))))

(defn solve [n]
  (+ 2 (func n 0)))