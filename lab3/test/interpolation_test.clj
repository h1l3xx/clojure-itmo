(ns interpolation-test
  (:require [clojure.test :refer :all]
            [interpolation.interpolation :refer :all]))

(deftest test-parse-point
  (testing "parse-point function"
    (is (= [1.0 2.0] (parse-point "1.0 2.0")))
    (is (= [1.1 2.2] (parse-point "1.1\t2.2")))
    (is (= [3.0 4.0] (parse-point "3.0;4.0")))))

(deftest test-interpolate-linear
  (testing "interpolate-linear function"
    (let [data [[1.0 2.0] [3.0 4.0]]
          step-size 0.5
          result (interpolate-linear data step-size)]
      (is (= 5 (count result)))
      (is (= [(double 1.0) 2.0] (first result)))
      (is (= [(double 3.0) 4.0] (last result)))
      (is (= [(double 2.0) 3.0] (nth result 2))))))

(deftest test-lagrange
  (testing "lagrange function"
    (let [data [[1.0 2.0] [2.0 4.0] [3.0 6.0]]
          x 2.5
          result (lagrange data x)]
      (is (= 5.0 result)))))

(deftest test-interpolate-lagrange
  (testing "interpolate-lagrange function"
    (let [data [[1.0 2.0] [2.0 4.0] [3.0 6.0]]
          step-size 0.5
          start 1.0
          end 3.0
          result (interpolate-lagrange data step-size start end)]
      (is (= 5 (count result)))
      (is (= [(double 1.0) 2.0] (first result)))
      (is (= [(double 3.0) 6.0] (last result))))))
