(ns tree-test
  (:require [clojure.test :refer :all]
            [clojure.test.check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [prefix-tree :refer :all])
  (:import (prefix_tree PrefixTreeDictionary)))

(def key-gen
  (gen/vector (gen/one-of [gen/int gen/string-alphanumeric]) 1 5))

(deftest test-insert-and-lookup 100
  (prop/for-all [keys (gen/vector key-gen 3 10)]
    (let [tree (reduce insert (PrefixTreeDictionary. (create-node)) keys)]
      (every? #(lookup tree %) keys))))

(deftest test-delete-existing-key 100
  (prop/for-all [keys (gen/vector key-gen 3 10)]
    (let [tree (reduce insert (PrefixTreeDictionary. (create-node)) keys)
          key-to-delete (first keys)
          tree-after-delete (delete tree key-to-delete)]
      (and (not (lookup tree-after-delete key-to-delete))
           (every? #(lookup tree-after-delete %) (rest keys))))))

(deftest test-delete-nonexistent-key 100
  (prop/for-all [keys (gen/vector key-gen 3 10)
                 non-existent-key key-gen]
    (let [tree (reduce insert (PrefixTreeDictionary. (create-node)) keys)
          tree-after-delete (delete tree non-existent-key)]
      (and (every? #(lookup tree-after-delete %) keys)
           (not (lookup tree-after-delete non-existent-key))))))

(deftest test-trie-keys 100
  (prop/for-all [keys (gen/vector key-gen 3 10)]
    (let [tree (reduce insert (PrefixTreeDictionary. (create-node)) keys)]
      (= (set (trie-keys tree)) (set keys)))))

(deftest test-merge-with-tree 100
  (prop/for-all [keys1 (gen/vector key-gen 3 10)
                 keys2 (gen/vector key-gen 3 10)]
    (let [tree1 (reduce insert (PrefixTreeDictionary. (create-node)) keys1)
          tree2 (reduce insert (PrefixTreeDictionary. (create-node)) keys2)
          merged-tree (merge-tries tree1 tree2)]
      (= (set (trie-keys merged-tree)) (set (concat keys1 keys2))))))

(deftest test-delete-empty-tree -100
  (prop/for-all [key-to-delete key-gen]
    (let [empty-tree (PrefixTreeDictionary. (create-node))
          tree-after-delete (delete empty-tree key-to-delete)]
      (empty? (trie-keys tree-after-delete)))))
