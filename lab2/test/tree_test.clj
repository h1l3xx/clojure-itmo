(ns tree-test
  (:require [clojure.test :refer :all]
            [prefix-tree :refer :all])
  (:import (prefix_tree PrefixTreeDictionary)))

(deftest test-insert-and-lookup
  ;; Тестирование добавления и поиска ключей
  (let [tree (-> (PrefixTreeDictionary. (create-node))
                 (insert "cat")
                 (insert "car")
                 (insert "dog"))]

    ;; Проверка наличия добавленных ключей
    (is (true? (lookup tree "car")))
    (is (true? (lookup tree "car")))
    (is (true? (lookup tree "dog")))

    ;; Проверка отсутствия несуществующих ключей
    (is (false? (lookup tree "cow")))
    (is (false? (lookup tree "d")))
    (is (false? (lookup tree "do")))))

    ;; Крайние случаи: пустая строка
    (let [empty-tree (PrefixTreeDictionary. (create-node))]
      (is (false? (lookup empty-tree "")))
      (is (true? (lookup (insert empty-tree "") ""))))

(deftest test-delete-existing-key
  ;; Тестирование удаления существующего ключа
  (let [tree (-> (PrefixTreeDictionary. (create-node))
                 (insert "cat")
                 (insert "car")
                 (insert "dog"))
        tree-after-delete (delete tree "car")]

    (is (false? (lookup tree-after-delete "car")))
    (is (true? (lookup tree-after-delete "cat")))
    (is (true? (lookup tree-after-delete "dog")))))

(deftest test-delete-nonexistent-key
  ;; Тестирование удаления несуществующего ключа
  (let [tree (-> (PrefixTreeDictionary. (create-node))
                 (insert "cat")
                 (insert "car")
                 (insert "dog"))
        tree-after-delete (delete tree "cow")]

    (is (true? (lookup tree-after-delete "cat")))
    (is (true? (lookup tree-after-delete "car")))
    (is (true? (lookup tree-after-delete "dog")))))

(deftest test-delete-empty-string
  ;; Тестирование удаления пустой строки
  (let [tree (-> (PrefixTreeDictionary. (create-node))
                 (insert "cat")
                 (insert "car")
                 (insert "dog"))
        tree-with-empty-key (insert tree "")
        tree-after-delete-empty (delete tree-with-empty-key "")]

    (is (true? (lookup tree-with-empty-key "")))
    (is (false? (lookup tree-after-delete-empty "")))))

(deftest test-delete-empty-tree
  ;; Тестирование удаления из пустого дерева
  (let [empty-tree (PrefixTreeDictionary. (create-node))
        tree-after-delete (delete empty-tree "nonexistent-key")]

    (is (false? (lookup tree-after-delete "nonexistent-key"))) ;; должно быть nil
    (is (= (trie-keys tree-after-delete) ())))) ;; должно быть пустое дерево

(deftest test-trie-keys
  (testing "Получение всех ключей из дерева"
    (let [tree (-> (PrefixTreeDictionary. (create-node))
                   (insert [1 2 3])
                   (insert [4 5 6])
                   (insert [7 8 9])
                   (insert ["a" "b" "c"])
                   (insert [1 2 4]))]
      ;; Получение ключей
      (let [keys (trie-keys tree)]
        ;; Проверка ожидаемых ключей
        (is (= keys
               [[1 2 3] [1 2 4] [4 5 6] [7 8 9] ["a" "b" "c"]]))))))

(deftest test-merge-with-tree
  ;; Тестирование слияния деревьев
  (let [tree1 (-> (PrefixTreeDictionary. (create-node))
                  (insert "cat")
                  (insert "car"))
        tree2 (-> (PrefixTreeDictionary. (create-node))
                  (insert "car")
                  (insert "dog"))
        merged-tree (merge-tries tree1 tree2)]

    (println merged-tree)
    ;; Проверяем наличие ключей после слияния
    (is (true? (lookup merged-tree "cat")))
    (is (true? (lookup merged-tree "car")))
    (is (true? (lookup merged-tree "dog")))

    ;; Проверяем, что ключи из первого и второго дерева присутствуют
    (is (= (trie-keys merged-tree)) '("cat" "car" "dog"))

    ;; Проверяем слияние с пустым деревом
    (let [empty-tree (PrefixTreeDictionary. (create-node))
          merged-with-empty (merge-tries tree1 empty-tree)]
      (is (= (trie-keys merged-with-empty)) '("cat" "car")))))

(deftest test-merge-with-tree-no-keys
  ;; Проверяем слияние пустых деревьев
  (let [empty-tree1 (PrefixTreeDictionary. (create-node))
        empty-tree2 (PrefixTreeDictionary. (create-node))
        merged-tree (merge-tries empty-tree1 empty-tree2)]
    (is (= (trie-keys merged-tree) []))))

