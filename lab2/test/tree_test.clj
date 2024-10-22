(ns tree-test
  (:require [clojure.test :refer :all]
            [clojure.test.check]
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

(deftest test-create-prefix-tree-from-seq
  (testing
   (let [property (prop/for-all [keys (gen/vector string-gen 1 10)]
                                (let [trie (create-prefix-tree keys)]
                                  (every? (fn [key] (.search trie key)) keys)))]
     (is (clojure.test.check/quick-check 100 property))))

  (testing
   (let [property (prop/for-all [keys (gen/vector string-gen 1 10)
                                 missing-key string-gen]
                                (let [trie (create-prefix-tree keys)]
                                  (and (not (.search trie missing-key))
                                       (every? (fn [key] (.search trie key)) keys))))]
     (is (clojure.test.check/quick-check 100 property)))))

(deftest test-insert-and-search 100
  (prop/for-all [keys (gen/one-of [string-gen number-seq-gen symbol-seq-gen])]
                (let [tree (reduce insert (PrefixTreeDictionary. (create-node)) keys)]
                  (every? #(search tree %) keys))))

(deftest test-filter-keys
  (let [trie (create-prefix-tree ["cat" "car" "dog" "cart" "apple"])]
    (is (= (sort (filter-keys trie #(clojure.string/starts-with? % "ca")))
           ["car" "cart" "cat"]))
    (is (= (sort (filter-keys trie #(= (count %) 3)))
           ["car" "cat" "dog"]))
    (is (= (sort (filter-keys trie #(clojure.string/ends-with? % "e")))
           ["apple"]))))

(deftest test-delete-existing-key 100
  (prop/for-all [keys (gen/one-of [string-gen number-seq-gen symbol-seq-gen])]
                (let [tree (reduce insert (PrefixTreeDictionary. (create-node)) keys)
                      key-to-delete (first keys)
                      tree-after-delete (delete tree key-to-delete)]
                  (and (not (search tree-after-delete key-to-delete))
                       (every? #(search tree-after-delete %) (rest keys))))))

(deftest test-delete-nonexistent-key 100
  (prop/for-all [keys (gen/one-of [string-gen number-seq-gen symbol-seq-gen])
                 non-existent-key (gen/one-of [string-gen number-seq-gen symbol-seq-gen])]
                (let [tree (reduce insert (PrefixTreeDictionary. (create-node)) keys)
                      tree-after-delete (delete tree non-existent-key)]
                  (and (every? #(search tree-after-delete %) keys)
                       (not (search tree-after-delete non-existent-key))))))

(deftest test-trie-keys 100
  (prop/for-all [keys (gen/one-of [string-gen number-seq-gen symbol-seq-gen])]
                (let [tree (reduce insert (PrefixTreeDictionary. (create-node)) keys)]
                  (= (set (trie-keys tree)) (set keys)))))

(deftest test-merge-with-tree 100
  (prop/for-all [keys1 (gen/one-of [string-gen number-seq-gen symbol-seq-gen])
                 keys2 (gen/one-of [string-gen number-seq-gen symbol-seq-gen])]
                (let [tree1 (reduce insert (PrefixTreeDictionary. (create-node)) keys1)
                      tree2 (reduce insert (PrefixTreeDictionary. (create-node)) keys2)
                      merged-tree (merge-tries tree1 tree2)]
                  (= (set (trie-keys merged-tree)) (set (concat keys1 keys2))))))

(deftest test-delete-empty-tree 100
  (prop/for-all [key-to-delete (gen/one-of [string-gen number-seq-gen symbol-seq-gen])]
                (let [empty-tree (PrefixTreeDictionary. (create-node))
                      tree-after-delete (delete empty-tree key-to-delete)]
                  (empty? (trie-keys tree-after-delete)))))

(deftest test-commutative
  (testing "Check property A * B = B * A"
    (let [tree1 (reduce insert (PrefixTreeDictionary. (create-node)) (gen/sample string-gen 100))
          tree2 (reduce insert (PrefixTreeDictionary. (create-node)) (gen/sample string-gen 100))]
      (is (= true (compare-tries (merge-tries tree1 tree2) (merge-tries tree2 tree1)))))))

(deftest test-associate
  (testing "Check property A * (B * C) = (A * B) * C"
    (let [tree1 (reduce insert (PrefixTreeDictionary. (create-node)) (gen/sample string-gen 100))
          tree2 (reduce insert (PrefixTreeDictionary. (create-node)) (gen/sample string-gen 100))
          tree3 (reduce insert (PrefixTreeDictionary. (create-node)) (gen/sample string-gen 100))]
      (is (= true (compare-tries (merge-tries tree1 (merge-tries tree2 tree3))
                                 (merge-tries (merge-tries tree1 tree2) tree3)))))))

(deftest test-neutral-element
  (testing "Check property neutral element"
    (let [tree (reduce insert (PrefixTreeDictionary. (create-node)) (gen/sample string-gen 100))
          zero_tree (PrefixTreeDictionary. (create-node))]
      (is (= true (compare-tries (merge-tries tree zero_tree) tree))))))
