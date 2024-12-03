(ns interpolation.cli
  (:require [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  [["-s" "--step STEP" "Шаг для интерполяции" :parse-fn #(Double/parseDouble %)]
   ["-m" "--method METHOD" "Метод интерполяции (линейный, лагранж, оба)"
    :validate [#{"linear" "lagrange" "both"} "Укажите 'linear', 'lagrange' или 'both'"]]
   ["-w" "--window WINDOW" "размер окна для вычисления интерполяционного многочлена Лагранжа"]
   ["-h" "--help"]])

(defn parse-cli-args [args]
  (parse-opts args cli-options))