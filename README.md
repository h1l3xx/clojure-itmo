# Лабораторная работа №2

```
Выполнил: Маликов Александр Максимович P3322

Вариант: pre-dict
```

В ходе выполнения данной лабораторной работы, мною был реализован протокол ```IDictionary```, а также самое префиксное дерево ```PrefixTreeDictionary```, расширяющее функции протокола.

```clojure
(defprotocol IDictionary 
  (insert [this key] "Добавить ключ в словарь")
  (search [this key] "Проверить, существует ли ключ в словаре")
  (delete [this key] "Удалить ключ из словаря")
  (trie-keys [this] "Вернуть все ключи")
  (entries [this] "Вернуть все пары ключ-флаг")
  (merge-tries [this other] "Объединить два префиксных дерева")
  (compare-tries [this other] "Сравнение двух деревьев")
  (fold-left [this f] "Сложение элементов с левым обходом")
  (fold-right [this f] "Сложение элементов с правым обходом"))
```

## Функции префиксного дерева

### Вставка (insert)
```clojure
(insert [_ key]
    (letfn [(insert-seq [node elems]
              (if (empty? elems)
                (assoc node :is-end true)
                (let [elem (first elems)
                      next-node (get-in node [:children elem] (create-node))]
                  (assoc-in node [:children elem] (insert-seq next-node (rest elems))))))]
      (PrefixTreeDictionary. (insert-seq root key))))
```
Данная функция добавляет ключ в префиксное дерево. Ключ обрабатывается как последовательность элементов.
Функция использует рекурсивную внутреннюю функцию ```insert-seq```, которая:

- Проверяет, если элементы закончились, добавляет атрибут `:is-end true` в узел, обозначая конец ключа.

- Если элементы остались, добавляет первый элемент в поддерево, обновляя структуру узлов, пока все элементы не будут добавлены.

### Поиск слова (search)
```clojure
(search [_ key]
    (letfn [(lookup-seq [node elems]
              (if (empty? elems)
                (:is-end node)
                (let [next-node (get-in node [:children (first elems)])]
                  (if next-node
                    (lookup-seq next-node (rest elems))
                    false))))]
      (lookup-seq root key)))
```
Ищет ключ в дереве. Используется рекурсивная функция `lookup-seq`, которая:

- Проходит по узлам, соответствующим символам ключа.
- Если дошли до конца ключа, возвращает значение `:is-end`, которое указывает, является ли данный узел концом какого-либо ключа.
- Если соответствующий элемент не найден, возвращает `false`.

### Удаление (delete)
```clojure
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
```
Удаляет ключ из дерева. Функция использует рекурсивную функцию `remove-seq`, которая:

- Если элементы ключа закончились, сбрасывает флаг `:is-end` в `false`.
- Если есть дети узла, проверяет, можно ли удалить текущий узел, основываясь на том, что его дети пусты и сам узел не является концом другого ключа.
- Возвращает обновлённое дерево.

### Вывод всех ключей (trie-keys)
```clojure
(trie-keys [_]
    (letfn [(collect-keys [node prefix]
              (let [current-keys (if (:is-end node) [prefix] [])
                    children-keys (mapcat (fn [[elem child]]
                                            (collect-keys child (conj prefix elem)))
                                          (:children node))]
                (concat current-keys children-keys)))]
      (collect-keys root [])))
```

Возвращает все ключи, хранящиеся в префиксном дереве. Используется рекурсивная функция `collect-keys`, которая:

- Собирает все префиксы, которые помечены как завершённые ключи (с флагом `:is-end` `true`).
- Рекурсивно проходит по всем узлам, собирая ключи.

### Левая свертка (left)
```clojure
(fold-left [this f]
           (let [words (trie-keys this)
                 result (reduce f [] words)]
                (reduce (fn [trie key]
                            (.insert trie key))
                        (PrefixTreeDictionary. (create-node))
                        result)))
```
Функция `fold-left` возвращает новое префиксное дерево, содержащее результаты применения функции `f` к элементам исходного дерева. Порядок обработки элементов — слева направо.
Результат работы функции зависит от передаваемой пользователем функции.

### Правая свертка (right)
```clojure
(fold-right [this f]
    (let [words (reverse (trie-keys this))
          result (reduce f [] words)]
      (reduce (fn [trie key]
                (.insert trie key))
              (PrefixTreeDictionary. (create-node))
              result))))
```
Функция `fold-right` возвращает новое префиксное дерево, содержащее результаты применения функции `f` к элементам исходного дерева. Порядок обработки элементов — с право на лево.
Результат работы функции зависит от передаваемой пользователем функции.
### Вывод содержимого дерева в формате key-value (entries)
```clojure
(entries [_]
    (letfn [(collect-entries [node prefix]
              (if (nil? node)
                []
                (let [current-entry (if (:is-end node) [[prefix true]] [])
                      children-entries (mapcat (fn [[elem child]]
                                                 (collect-entries child (conj prefix elem)))
                                               (:children node))]
                  (concat current-entry children-entries))))]
      (collect-entries root [])))
```
Возвращает список пар `key-value`, где ключ — это путь по дереву, а значение — всегда `true` (поскольку дерево хранит только ключи). Использует рекурсивную функцию `collect-entries`, которая собирает такие пары по всем узлам.

### Сравнение (compare-tries)
```clojure
(compare-tries [tree1 tree2]
               (let [keys1 (trie-keys tree1)
                     keys2 (trie-keys tree2)]
                    (and (= (count keys1) (count keys2))
                         (every? #(and (search tree1 %) (search tree2 %)) keys1))))
```
Сравнивает два префиксных дерева по их ключам. Сначала собирает ключи из обоих деревьев с помощью `trie-keys`, затем проверяет, одинаково ли количество ключей и есть ли каждый ключ из первого дерева во втором.

### Объединение деревьев (merge-tries)
```clojure
(merge-tries [this other]
    (let [other-keys (trie-keys other)]
      (reduce (fn [tree key]
                (insert tree key))
              this
              other-keys)))
```
Объединяет два префиксных дерева, добавляя все ключи из второго дерева в первое. Для этого:

- Получает все ключи из второго дерева с помощью `trie-keys`.
- Вставляет их в текущее дерево (используя `insert`).

### Создание дерева по последовательности (create-prefix-tree)
```clojure
(defn create-prefix-tree [keys]
  (reduce (fn [trie key]
            (.insert trie key))
          (PrefixTreeDictionary. (create-node))
          keys))
```
Строится префиксное дерево на основе последовательности. `reduce`, чтобы поэтапно вставлять каждый ключ в дерево.

## Тестирование

Все тесты находятся [тут](lab2/test/).
Успешно проходят, что отражает работа CI.



