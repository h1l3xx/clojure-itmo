(ns interpolation.core
  (:require [interpolation.cli :refer [parse-cli-args]]
            [interpolation.interpolation :refer [parse-point interpolate-linear interpolate-lagrange format-results]]))

(defn process-data [step-size method window-size]
  (loop [points []]
    (let [input-line (read-line)]
      (if (nil? input-line)
        (do
          (println "Завершение.")
          (System/exit 0))
        (if (seq input-line)
          (let [parsed-point (parse-point input-line)
                updated-points (conj points parsed-point)
                sorted-points (if (every? (fn [[x1 x2]] (<= x1 x2)) (partition 2 1 (map first updated-points)))
                                      updated-points
                                      (do
                                        (println "Данные не отсортированы. Выполняется сортировка.")
                                        (sort-by first updated-points)))]

            (when (>= (count sorted-points) 2)
              (when (or (= method "linear") (= method "both"))
                (let [window (take-last 2 sorted-points)
                      linear-result (interpolate-linear window step-size)
                      x-values (map first linear-result)
                      y-values (map second linear-result)]
                  (println (format "Линейная интерполяция: от %s до %s"
                                   (first x-values) (last x-values)))
                  (println (format-results x-values y-values)))))

            (doseq [group (partition window-size 1 sorted-points)]
              (let [start (first (map first group))
                    finish (last (map first group))
                    lagrange-result (interpolate-lagrange group step-size start finish)
                    x-values (map first lagrange-result)
                    y-values (map second lagrange-result)]
                (println (format "Интерполяционный многочлен Лагранжа: от %s до %s"
                                 start finish))
                (println (format-results x-values y-values))))

            (recur sorted-points))
          (recur points))))))

(defn -main [& args]
  (let [{:keys [options]} (parse-cli-args args)]
    (if (:help options)
      (println "Использование: \n  -s  Шаг интерполяции \n  -m  Метод интерполяции (linear, lagrange, both) \n  -w  Размер окна для Лагранжа")
      (let [step-size (or (:step options) 1.0)
            method (or (:method options) "both")
            window-size (Integer/parseInt (or (:window options) "5"))]
        (println "Введите данные в формате X Y")
        (process-data step-size method window-size)))))