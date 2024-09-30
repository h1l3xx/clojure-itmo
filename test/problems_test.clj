(ns problems-test
  (:require [clojure.test :refer [deftest is]]
            [prob10.modular :as problem-10-modular]
            [prob10.lazy :as problem-10-lazy]))

(deftest test-problem-10-modular-solve
  (is (= (problem-10-modular/solve 2000000) 142913828922)))
(deftest test-problem-10-lazy-solve
  (is (= (problem-10-lazy/solve 2000000) 142913828922)))

