(ns eopl.core.show-tree)

(defn show-tree
  ([before prefix tree]
     (let [[x left right] tree]
       (str prefix x "\n"
            (if (seq left)
              (show-tree (str before
                              (if (seq right) "|  " "   "))
                         (str before  "|--") left))
            (if (seq right)
              (show-tree (str before "   ")   (str before "`--") right)))))

  ([tree] (show-tree "" "" tree)))

(defn print-tree [tree]
  (print (show-tree tree)))

(print-tree '(1 (2 (3 () (4 (5) (6))) (10)) (3 (4 (9 (10) (11))) (5))))

;; 1
;; |--2
;; |  |--3
;; |  |  `--4
;; |  |     |--5
;; |  |     `--6
;; |  `--10
;; `--3
;;    |--4
;;    |  |--9
;;    |     |--10
;;    |     `--11
;;    `--5




