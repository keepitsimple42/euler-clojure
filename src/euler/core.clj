(ns euler.core
  (:gen-class)
 
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Project Euler Problems"))

;problem 1
(apply +
       (filter #(or (= (mod % 3) 0) (= (mod % 5) 0))
               (range 1 1000)))


;problem 2
(def lazy-fib
  (map first (iterate (fn [[a b]] [b (+' a b)] ) [1 1])))

;(take 10 lazy-fib)

(reduce + (filter even? (take-while #(< % 4000000)  lazy-fib)))


;primes
(reduce
 (fn [primes number]
   (if (some zero? (map (partial mod number) primes))
     primes
     (conj primes number)))
 [2]
 (take 10000 (iterate inc 3))) 

;lazy-seq

(defn pos-nums
  ([] (pos-nums 1))
  ([n] (lazy-seq (cons n (pos-nums (inc n))))))

(take 5 (pos-nums))


;can we make lazy primes list?



(defn gen-primes "Generates an infinite, lazy sequence of prime numbers"

  []

  (letfn [(reinsert [table x prime]

            (update-in table [(+ prime x)] conj prime))

          (primes-step [table d]

            (if-let [factors (get table d)]

              (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)

                     (inc d))

              (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))

                                             (inc d))))))]

    (primes-step {} 2)))

(take 10000 (gen-primes))


;problem 3 largest prime factor

(defn greatest-prime-of [n]
  (reduce max (filter #(zero? (mod n %))
          (take-while #(< % (Math/sqrt n)) (gen-primes)))))

(greatest-prime-of 600851475143)
;(greatest-prime-of 33)
(take-while #(< % 100) (gen-primes))



;problem 3

(defn palindrome? [n]
  (= n (reverse n)))
(defn palindrome-number? [n]
  (palindrome? (seq (str n))))

(reduce max (filter palindrome-number?
                    (for [i (range 100 1000) j (range i 1000)] (* i j))))


;problem 4

(defn gcd [n k]
  (loop [a n b k]
    (if (zero? b) a (recur b (mod a b)))))
(defn lcm [n k]
  (/ (Math/abs (* n k)) (gcd n k)))

(reduce lcm (range 1 21))




;problem 7

(last (take 10001 (gen-primes)))



;finding factors - seems infeasibly fast

(defn factors [n]
  (into (sorted-set)
        (reduce concat
                (for [x (range 1 (inc (Math/sqrt n))) :when (zero? (rem n x))]
                  [x (/ n x)]))))



;prime factors

(defn prime-factors
"Return a list of factors of N."
([n]
 (prime-factors n 2 ()))
([n k acc]
 (if (= 1 n)
   acc
   (if (= 0 (rem n k))
     (recur (quot n k) k (cons k acc))
             (recur n (inc k) acc)))))

(prime-factors 6000120004)

(prime-factors(reduce * (take 15 (gen-primes))))






