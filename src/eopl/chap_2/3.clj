(ns eopl.chap-2.3
  (:use clojure.test))

(def one '(one))

(defn diff [left right]
  (list 'diff left right))

(def zero (diff one one))

(defn predecessor [n]
  (diff n one))

(defn minus [n]
  (diff zero n))

(defn successor [n]
  (diff n (minus one)))

(defn diff-tree-plus [a b]
  (diff a (minus b)))

(defn value-of [n]
  (cond (= '(one) n) 1
        (= 'diff (first n)) (let [[_ left right] n]
                              (- (value-of left) (value-of right)))))

(defn one? [n]
  ( = one n))

(defn minus-one? [n]
  (= (minus one) n))

(defn zero? [n]
  (= zero n))

(defn consistent-inc [n]
  (if (one? n)
    (diff n (minus one))
    (let [[_ left right] n]
      (if (minus-one? right)
        (diff n (minus one))
        left))))

(defn consistent-dec [n]
  (if (one? n)
    (diff n one)
    (let [[_ left right] n]
      (if (minus-one? right)
        left
        (diff n one)))))


(defn diff? [n]
  (= 'diff (first n)))

(defn consistent-rep
  ([n]
     (consistent-rep n zero true))
  ([n rep increment]
     (if (diff? n)
       (let [[_ left right] n]
         (consistent-rep right (consistent-rep left rep increment) (not increment)))
       (if increment
         (consistent-inc rep)
         (consistent-dec rep)))))

(defn is-zero? [n]
  (zero? (consistent-rep n)))

(deftest number-repr
  (is (is-zero? (successor (successor (predecessor (predecessor (predecessor (predecessor (predecessor (successor (successor one)))))))))))
  (is (= (consistent-inc (consistent-inc (consistent-dec (consistent-dec (consistent-dec (consistent-dec (consistent-inc (consistent-inc zero))))))))
         zero)))

(run-tests)
