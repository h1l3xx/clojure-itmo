(ns prefix-tree)

(defn create-node []
  {:is-end false :children {}})

(defprotocol IDictionary
  (insert [this key] "Добавить ключ в словарь")
  (lookup [this key] "Проверить, существует ли ключ в словаре")
  (delete [this key] "Удалить ключ из словаря")
  (trie-keys [this] "Вернуть все ключи")
  (entries [this] "Вернуть все пары ключ-флаг"))

(deftype PrefixTreeDictionary [root]
  IDictionary
  ;; Вставка ключа в префиксное дерево
  (insert [this key]
    (letfn [(insert-seq [node chars]
              (if (empty? chars)
                (assoc node :is-end true)  ;; Устанавливаем флаг конца слова
                (let [c (first chars)
                      next-node (get-in node [:children c] (create-node))]
                  (assoc-in node [:children c] (insert-seq next-node (rest chars))))))]
      (PrefixTreeDictionary. (insert-seq root key))))

  ;; Проверка наличия ключа в префиксном дереве
  (lookup [this key]
    (letfn [(lookup-seq [node chars]
              (if (empty? chars)
                (:is-end node)  ;; Возвращаем флаг окончания слова
                (let [next-node (get-in node [:children (first chars)])]
                  (when next-node
                    (lookup-seq next-node (rest chars))))))]
      (lookup-seq root key)))

  ;; Удаление ключа из префиксного дерева
  (delete [this key]
    (letfn [(remove-seq [node chars]
              (if (empty? chars)
                (assoc node :is-end false)  ;; Убираем флаг конца слова
                (let [c (first chars)
                      next-node (get-in node [:children c])]
                  (when next-node
                    (let [updated-node (remove-seq next-node (rest chars))]
                      (if (and (not (:is-end updated-node))
                               (empty? (:children updated-node)))
                        (update node :children dissoc c)  ;; Удаляем узел, если он пуст
                        (assoc-in node [:children c] updated-node)))))))]
      (PrefixTreeDictionary. (remove-seq root key))))

  ;; Получение всех ключей
  (trie-keys [this]
    (letfn [(collect-keys [node prefix]
              (let [current-keys (if (:is-end node) [prefix] [])
                    children-keys (mapcat (fn [[char child]]
                                            (collect-keys child (conj prefix char)))
                                          (:children node))]
                (concat current-keys children-keys)))]
      (collect-keys root [])))

  ;; Получение всех пар ключ-флаг
  (entries [this]
    (letfn [(collect-entries [node prefix]
              (let [current-entry (if (:is-end node) [[prefix true]] [])
                    children-entries (mapcat (fn [[char child]]
                                               (collect-entries child (conj prefix char)))
                                             (:children node))]
                (concat current-entry children-entries)))]
      (collect-entries root []))))
