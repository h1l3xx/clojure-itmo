(ns prefix-tree)

(defn create-node []
  {:is-end false :children {}})

(defprotocol IDictionary
  (insert [this key] "Добавить ключ в словарь")
  (lookup [this key] "Проверить, существует ли ключ в словаре")
  (delete [this key] "Удалить ключ из словаря")
  (trie-keys [this] "Вернуть все ключи")
  (entries [this] "Вернуть все пары ключ-флаг")
  (merge-tries [this other] "Объединить два префиксных дерева")
  (left [this] "Левый обход (снизу вверх)")
  (right [this] "Правый обход (сверху вниз)"))

(deftype PrefixTreeDictionary [root]
  IDictionary
  ;; Вставка ключа в префиксное дерево
  (insert [_ key]
    (letfn [(insert-seq [node elems]
              (if (empty? elems)
                (assoc node :is-end true)  ;; Устанавливаем флаг конца ключа
                (let [elem (first elems)
                      next-node (get-in node [:children elem] (create-node))]
                  (assoc-in node [:children elem] (insert-seq next-node (rest elems))))))]
      (PrefixTreeDictionary. (insert-seq root key))))

  ;; Проверка наличия ключа в префиксное дерево
  (lookup [_ key]
    (letfn [(lookup-seq [node elems]
              (if (empty? elems)
                (:is-end node)  ;; Возвращаем флаг окончания ключа
                (let [next-node (get-in node [:children (first elems)])]
                  (if next-node
                    (lookup-seq next-node (rest elems))
                    false))))]  ;; Если узел не найден, возвращаем false
      (lookup-seq root key)))

  ;; Удаление ключа из дерева
  (delete [this key]
    (if (empty? (:children root))
      ;; Если дерево пустое, возвращаем его без изменений
      this
      (letfn [(remove-seq [node elems]
                (if (empty? elems)
                  (assoc node :is-end false)  ;; Убираем флаг конца ключа
                  (let [elem (first elems)
                        next-node (get-in node [:children elem])]
                    (if next-node
                      (let [updated-node (remove-seq next-node (rest elems))]
                        (if (and (not (:is-end updated-node))
                                 (empty? (:children updated-node)))
                          (update node :children dissoc elem)  ;; Удаляем узел, если он пуст
                          (assoc-in node [:children elem] updated-node)))
                      node))))]
        (PrefixTreeDictionary. (remove-seq root key)))))

  ;; Получение всех ключей
  (trie-keys [_]
    (letfn [(collect-keys [node prefix]
              (let [current-keys (if (:is-end node) [prefix] [])  ;; Ключи хранятся в виде коллекций
                    children-keys (mapcat (fn [[elem child]]
                                            (collect-keys child (conj prefix elem)))
                                          (:children node))]
                (concat current-keys children-keys)))]
      (collect-keys root [])))
  (left [this]
    (letfn [(collect-keys [node prefix]
              (if (nil? node)
                []
                (let [current-keys (if (:is-end node) [(apply str (concat prefix))] [])
                      children-keys (mapcat (fn [[char child]]
                                              (collect-keys child (conj prefix char)))
                                            (:children node))]
                  (concat children-keys current-keys))))]
      (collect-keys root [])))

  ;; Правый обход (сверху вниз)
  (right [this]
    (letfn [(collect-keys [node prefix]
              (if (nil? node)
                []
                (let [current-keys (if (:is-end node) [(apply str (concat prefix))] [])
                      children-keys (mapcat (fn [[char child]]
                                              (collect-keys child (conj prefix char)))
                                            (:children node))]
                  (concat current-keys children-keys))))]
      (collect-keys root [])))
  ;; Получение всех пар ключ-флаг
  (entries [_]
    (letfn [(collect-entries [node prefix]
              (if (nil? node)
                [] ;; Если узел nil, возвращаем пустой список
                (let [current-entry (if (:is-end node) [[prefix true]] [])  ;; Ключи в виде коллекций
                      children-entries (mapcat (fn [[elem child]]
                                                 (collect-entries child (conj prefix elem)))
                                               (:children node))]
                  (concat current-entry children-entries))))]
      (collect-entries root [])))

  ;; Функция для объединения двух деревьев через вставку ключей
  (merge-tries [this other]
    (let [other-keys (trie-keys other)]
      (reduce (fn [tree key]
                (insert tree key))
              this
              other-keys))))
