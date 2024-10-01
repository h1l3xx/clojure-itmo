(ns problems-test
  (:require [clojure.test :refer [deftest is]]
            [prob10.modular :as problem-10-modular]
            [prob10.lazy :as problem-10-lazy]
            [prob10.tail :as problem-10-tail]
            [prob21.loop :as problem-21-loop]
            [prob21.map :as problem-21-map]))

(deftest test-problem-10-modular-solve
  (is (= (problem-10-modular/solve 2000000) 142913828922)))
(deftest test-problem-10-lazy-solve
  (is (= (problem-10-lazy/solve 2000000) 142913828922)))
(deftest test-problem-10-tail-solve
  (is (= (problem-10-tail/solve 2000000) 142913828922)))
(deftest test-problem-21-loop-solve
  (is (= (problem-21-loop/solve 10000) 31626)))
(deftest test-problem-21-map-solve
  (is (= (problem-21-map/solve 10000) 31626)))

