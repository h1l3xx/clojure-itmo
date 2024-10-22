(ns tree-polymorphism-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [prefix-tree :refer :all])
  (:import (prefix_tree PrefixTreeDictionary)))

(def string-gen
  (gen/fmap #(apply str %) (gen/vector gen/char-alpha 1 10)))

(def number-seq-gen
  (gen/vector (gen/choose 0 1000) 1 5))

(def symbol-seq-gen
  (gen/vector (gen/fmap #(symbol (str %)) (gen/elements (map char (range 97 123)))) 1 5))

(def mixed-gen
  (gen/one-of [string-gen number-seq-gen symbol-seq-gen]))

(deftest test-polymorphic-insert-and-search-string
  (tc/quick-check 100
                  (prop/for-all [key string-gen]
                                (let [tree (PrefixTreeDictionary. (create-node))
                                      dict (insert tree key)]
                                  (and (search dict key)
                                       (not (search dict "nothing")))))))

(deftest test-polymorphic-insert-and-search-number
  (tc/quick-check 100
                  (prop/for-all [key number-seq-gen]
                                (let [tree (PrefixTreeDictionary. (create-node))
                                      dict (insert tree key)]
                                  (and (search dict key)
                                       (not (search dict [9999])))))))

(deftest test-polymorphic-insert-and-search-symbols
  (tc/quick-check 100
                  (prop/for-all [key symbol-seq-gen]
                                (let [tree (PrefixTreeDictionary. (create-node))
                                      dict (insert tree key)]
                                  (and (search dict key)
                                       (not (search dict ['x 'y 'z])))))))

(deftest test-polymorphic-insert-and-search-mixed
  (tc/quick-check 100
                  (prop/for-all [key mixed-gen]
                                (let [tree (PrefixTreeDictionary. (create-node))
                                      dict (insert tree key)]
                                  (and (search dict key)
                                       (not (search dict ["nothing" 123])))))))

(deftest test-polymorphic-merge-tries
  (tc/quick-check 100
                  (prop/for-all [keys1 (gen/vector mixed-gen 1 5)
                                 keys2 (gen/vector mixed-gen 1 5)]
                                (let [tree1 (reduce insert (PrefixTreeDictionary. (create-node)) keys1)
                                      tree2 (reduce insert (PrefixTreeDictionary. (create-node)) keys2)
                                      merged-tree (merge-tries tree1 tree2)]
                                  (and (every? #(search merged-tree %) keys1)
                                       (every? #(search merged-tree %) keys2))))))

(deftest test-polymorphic-compare-tries
  (tc/quick-check 100
                  (prop/for-all [keys1 (gen/vector mixed-gen 1 5)
                                 keys2 (gen/vector mixed-gen 1 5)]
                                (let [tree1 (reduce insert (PrefixTreeDictionary. (create-node)) keys1)
                                      tree2 (reduce insert (PrefixTreeDictionary. (create-node)) keys1)
                                      tree3 (reduce insert (PrefixTreeDictionary. (create-node)) keys2)]
                                  (and (compare-tries tree1 tree2)
                                       (not (compare-tries tree1 tree3)))))))

