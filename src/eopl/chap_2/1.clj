;; Implement the four required operations for bigits. Then use your
;; implementation to calculate the factorial of 10. How does the
;; execution time vary as this argument changes? How does the excution
;; time vary as the base changes? Explain why.

(ns eopl.chap-2.1
  (:use clojure.test))

(defn base [] 16)

(defn zero []
  (list (base) 0))

(defn is-zero? [n]
  (= (zero) n))

(defn successor [n]
  (let [base (first n)
        lsb (second n)
        r (rest (rest n))]
    (if (= lsb (dec base))
      (if (empty? r)
        (list base 0 1)
        (cons base (cons 0 (rest (successor (cons base r))))))
      (cons base (cons (inc lsb) r)))))

(defn predecessor [n]
  (let [base (first n)
        lsb (second n)
        r (rest (rest n))]
    (if (= lsb 0)
      (if (empty? r)
        (list base 0)
        (cons base (cons (dec base) (rest (predecessor (cons base r))))))
      (cons base (cons (dec lsb) r)))))

(deftest bigits-test
  (is (is-zero? (predecessor (successor (zero))))))

(run-tests)
