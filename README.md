# Лабораторная работа №1

```
Выполнил: Маликов Александр Максимович P3322

Вариант: 10, 21
```

## Проблема 10

### Описание

Сумма простых чисел меньше 10 равна 2 + 3 + 5 + 7 = 17.

Найдите сумму всех простых чисел меньше двух миллионов.

### Реализация

Решение этой задачи было реализованно тремя разными способами:

1. Ленивые последовательности.
2. Хвостовая рекурсия.
3. Модульная реализация (генерация последовательности, фильтрация и свёртка).

#### Реализация на основе ленивых последовательностей

``` ./src/prob10/lazy.clj ```

Создается последовательность от 2 до n при помощи iterate, из которой удаляются те числа,
что делятся хотя бы на какое-то уже найденное простое число

```clojure
 (def primes
   (remove
     (fn [x]
       (some #(zero? (mod x %)) (take (Math/sqrt x) primes)))
     (iterate inc 2)))

(defn solve [n]
  (apply + (take-while #(<= % n) primes)))
 ```


#### Реализация на основе хвостовой рекурсии

``` ./src/prob10/tail.clj ```

Clojure предлагает конструкцию loop, который работает на базе хвостовой рекурсии

Данная реализация построена на базе решета Эратосфена

Генерируем мапу "число" -> true \ false (Простое?)

```clojure
 (defn func [nums value]
   (if (zero? (mod nums 2))
     (recur (- nums 1) value)
     (if (<= nums 1)
       value
       (if (prime? nums)
         (recur (- nums 2) (+ value nums))
         (recur (- nums 2) value)))))
 ```

#### Модульная реализация 
```clojure
(defn prime? [n]
  (and (> n 1)
       (not-any? #(zero? (mod n %)) (range 2 (inc (Math/sqrt n))))))

(def primes
  (filter prime?
          (iterate inc 2)))

(defn solve [n]
  (apply + (cons 2 (take-while #(< % n) primes))))

```

## Проблема 21

### Описание

Пусть d(n) определяется как сумма делителей n (числа меньше n, делящие n нацело).
Если d(a) = b и d(b) = a, где a ≠ b, то a и b называются дружественной парой, а каждое из чисел a и b - дружественным числом.

Например, делителями числа 220 являются 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 и 110, поэтому d(220) = 284. Делители 284 - 1, 2, 4, 71, 142, поэтому d(284) = 220.

Подсчитайте сумму всех дружественных чисел меньше 10000.

### Реализация

В рамках решения этой проблемы были разработаны две различные реализации

#### Реализация на основе спец. синтаксиса для циклов

```clojure
(defn sum-of-divisors [n]
  (loop [i 1 acc 0]
    (if (>= i n)
      acc
      (if (zero? (mod n i))
        (recur (inc i) (+ acc i))
        (recur (inc i) acc)))))

(defn sum-friendly-numbers [limit]
  (loop [n 2 acc 0]
    (if (>= n limit)
      acc
      (let [pair-sum (sum-of-divisors n)]
        (if (and (not= n pair-sum) (= n (sum-of-divisors pair-sum)))
          (recur (inc n) (+ acc n))
          (recur (inc n) acc))))))
```


#### Решение с использованием map.


```clojure
(defn sum-divisors [n]
  (let [divisors (for [i (range 1 (inc (Math/sqrt n)))
                       :when (zero? (mod n i))]
                   (if (= i (quot n i))
                     i
                     (list i (quot n i))))]
    (->> divisors
         flatten
         (remove #{n})
         (distinct)
         (apply +))))

(defn friendly-numbers [limit]
  (let [d (into {} (map (fn [n] [n (sum-divisors n)]) (range 1 limit)))]
    (->> d
         (filter (fn [[a b]] (and (not= a b) (= (get d b) a))))
         (map first)
         (distinct)
         (apply +))))
```

## Вывод
TODO