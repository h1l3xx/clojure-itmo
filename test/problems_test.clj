(ns problems-test
  (:require [clojure.test :refer [deftest is]]
            [prob10.modular :as problem-10-modular]
            [prob10.lazy :as problem-10-lazy]
            [prob21.tail_recur :as problem-21-tail]))

(deftest test-problem-10-modular-solve
  (is (= (problem-10-modular/solve 2000000) 142913828922)))
(deftest test-problem-10-lazy-solve
  (is (= (problem-10-lazy/solve 2000000) 142913828922)))
(deftest test-problem-21-tail-recur-solve
  (is (= (problem-21-tail/solve 10000) 31626)))

