(ns prefix-tree)

(defn create-node []
  {:is-end false :children {}})

(defprotocol IDictionary
  (insert [this key] "Добавить ключ в словарь")
  (search [this key] "Проверить, существует ли ключ в словаре")
  (delete [this key] "Удалить ключ из словаря")
  (trie-keys [this] "Вернуть все ключи")
  (entries [this] "Вернуть все пары ключ-флаг")
  (merge-tries [this other] "Объединить два префиксных дерева")
  (compare-tries [this other] "Сравнение двух деревьев")
  (fold-left [this f] "Сложение элементов с левым обходом")
  (fold-right [this f] "Сложение элементов с правым обходом")
  (filter-keys [this predicate]) "Фильтрация дерева по предикату")

(deftype PrefixTreeDictionary [root] IDictionary
  (insert [_ key]
    (letfn [(insert-seq [node elems]
              (if (empty? elems)
                (assoc node :is-end true)
                (let [elem (first elems)
                      next-node (get-in node [:children elem] (create-node))]
                  (assoc-in node [:children elem] (insert-seq next-node (rest elems))))))]
      (PrefixTreeDictionary. (insert-seq root key))))

  (search [_ key]
    (letfn [(lookup-seq [node elems]
              (if (empty? elems)
                (:is-end node)
                (let [next-node (get-in node [:children (first elems)])]
                  (if next-node
                    (lookup-seq next-node (rest elems))
                    false))))]  ;; Если узел не найден, возвращаем false
      (lookup-seq root key)))

  (delete [this key]
    (if (empty? (:children root))
      this
      (letfn [(remove-seq [node elems]
                (if (empty? elems)
                  (assoc node :is-end false)
                  (let [elem (first elems)
                        next-node (get-in node [:children elem])]
                    (if next-node
                      (let [updated-node (remove-seq next-node (rest elems))]
                        (if (and (not (:is-end updated-node))
                                 (empty? (:children updated-node)))
                          (update node :children dissoc elem)
                          (assoc-in node [:children elem] updated-node)))
                      node))))]
        (PrefixTreeDictionary. (remove-seq root key)))))

  (trie-keys [_]
    (letfn [(collect-keys [node prefix]
              (let [current-keys (if (:is-end node) [prefix] [])  ;; Ключи хранятся в виде коллекций
                    children-keys (mapcat (fn [[elem child]]
                                            (collect-keys child (conj prefix elem)))
                                          (:children node))]
                (concat current-keys children-keys)))]
      (collect-keys root [])))

  (entries [_]
    (letfn [(collect-entries [node prefix]
              (if (nil? node) []
                (let [current-entry (if (:is-end node) [[(apply str prefix) true]] [])
                      children-entries (mapcat (fn [[elem child]]
                                                 (collect-entries child (conj prefix elem)))
                                               (:children node))]
                  (concat current-entry children-entries))))]
      (collect-entries root [])))

  (compare-tries [tree1 tree2]
    (let [keys1 (trie-keys tree1)
          keys2 (trie-keys tree2)]
      (and (= (count keys1) (count keys2))
           (every? #(and (search tree1 %) (search tree2 %)) keys1))))

  (merge-tries [this other]
    (let [other-keys (trie-keys other)]
      (reduce (fn [tree key]
                (insert tree key))
              this
              other-keys)))

  (fold-left [this f]
    (let [words (trie-keys this)
          result (reduce f [] words)]
      (reduce (fn [trie key]
                (.insert trie key))
              (PrefixTreeDictionary. (create-node))
              result)))

  (fold-right [this f]
    (let [words (reverse (trie-keys this))
          result (reduce f [] words)]
      (reduce (fn [trie key]
                (.insert trie key))
              (PrefixTreeDictionary. (create-node))
              result)))
  (filter-keys [_ pred]
    (letfn [(collect-filtered-keys [node prefix]
              (let [current-key (apply str prefix)
                    current-keys (if (and (:is-end node) (pred current-key))
                                   [current-key]
                                   [])
                    children-keys (mapcat (fn [[elem child]]
                                            (collect-filtered-keys child (conj prefix elem)))
                                          (:children node))]
                (concat current-keys children-keys)))]
      (collect-filtered-keys root []))))

(defn create-prefix-tree [keys]
  (reduce (fn [trie key]
            (.insert trie key))
          (PrefixTreeDictionary. (create-node))
          keys))