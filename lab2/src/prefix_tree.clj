(ns prefix_tree)

(defn create-node []
  {:is-end false :children {}})

(defn insert-word [trie word]
  (letfn [(insert-char [node chars]
            (if (empty? chars)
              (assoc node :is-end true)
              (let [c (first chars)
                    next-node (get-in node [:children c] (create-node))]
                (assoc-in node [:children c] (insert-char next-node (rest chars))))))]
    (insert-char trie word)))

(defn remove-word [trie word]
  (letfn [(remove-char [node chars]
            (if (empty? chars)
              (if (:is-end node)
                (assoc node :is-end false)
                node)
              (let [c (first chars)
                    next-node (get-in node [:children c])]
                (if next-node
                  (let [updated-node (remove-char next-node (rest chars))]
                    (if (and (empty? (:children updated-node))
                             (not (:is-end updated-node)))
                      (update node :children dissoc c)
                      (assoc-in node [:children c] updated-node)))
                  node))))]
    (remove-char trie word)))

(defn search-word [trie word]
  (letfn [(search-char [node chars]
            (cond
              (nil? node) false
              (empty? chars) (:is-end node)
              :else (recur (get-in node [:children (first chars)]) (rest chars))))]
    (search-char trie word)))

(defn search-prefix [trie prefix]
  (letfn [(search-prefix [node chars]
            (cond
              (nil? node) false
              (empty? chars) true
              :else (recur (get-in node [:children (first chars)]) (rest chars))))]
    (search-prefix trie prefix)))

(defn filter-trie [trie pred]
  (letfn [(filter-node [node prefix]
            ;; Проверяем текущий узел
            (let [filtered-children
                  (into {} (for [[char child] (:children node)
                                 :let [filtered-child (filter-node child (str prefix char))]
                                 :when (or (:is-end filtered-child)
                                           (not (empty? (:children filtered-child))))]
                             [char filtered-child]))]
              ;; Возвращаем обновленный узел, если он содержит соответствующее слово
              (if (and (:is-end node) (pred prefix))
                ;; Сохраняем узел, если слово удовлетворяет предикату
                {:is-end true :children filtered-children}
                ;; Если узел не является концом нужного слова, просто возвращаем его детей
                {:is-end false :children filtered-children})))]
    (filter-node trie "")))

(defn build-trie  [words]
  (reduce insert-word (create-node) words))

(defn trie-to-words [trie]
  (letfn [(collect-words [node prefix]
            (let [current-word (if (:is-end node) [prefix] [])
                  children-words (mapcat (fn [[char child]]
                                           (collect-words child (str prefix char)))
                                         (:children node))]
              (concat current-word children-words)))]
    (collect-words trie "")))

(defn merge-tries [trie1 trie2]
  (letfn [(merge-nodes [node1 node2]
            ;; Сливаем флаги конца слова
            (let [is-end (or (:is-end node1) (:is-end node2))
                  ;; Сливаем дочерние узлы
                  merged-children (merge-with merge-nodes
                                              (:children node1 {})
                                              (:children node2 {}))]
              {:is-end is-end :children merged-children}))]
    (merge-nodes trie1 trie2)))


(defn tries-equal?  [trie1 trie2]
  (letfn [(compare-nodes [node1 node2]
            ;; Проверяем флаг конца слова
            (and (= (:is-end node1) (:is-end node2))
                 ;; Проверяем, что дочерние узлы одинаковы
                 (= (set (keys (:children node1)))
                    (set (keys (:children node2))))
                 ;; Рекурсивно сравниваем дочерние узлы
                 (every? (fn [char]
                           (compare-nodes (get (:children node1) char)
                                          (get (:children node2) char)))
                         (keys (:children node1)))))]
    (compare-nodes trie1 trie2)))
