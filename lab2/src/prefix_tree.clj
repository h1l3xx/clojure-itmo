(ns prefix_tree)

(defn create-node
  []
  {:is-end false :children {}})

(defn insert-word [trie word]
  (letfn [(insert-char [node chars]
            (if (empty? chars)
              (assoc node :is-end true)
              (let [c (first chars)
                    next-node (get-in node [:children c] (create-node))]
                (assoc-in node [:children c] (insert-char next-node (rest chars))))))]
    (insert-char trie word)))

(defn remove-word
  [trie word]
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

(def trie (create-node))

