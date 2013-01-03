(ns eopl.core.define-datatype
  (:use clojure.test))

(defmacro data-type-predicate [type-name type-predicate-name]
  `(defn ~type-predicate-name [~'variant]
     (= (:type ~'variant) '~type-name)))

(defmacro data-type-variant [variant type-name]
  (let [variant-name (first variant)
        variant-spec (rest variant)
        variant-field-names (map first variant-spec)
        variant-args (mapcat #(reverse (take (if (= (second %1) '&) 2 1) %1)) variant-spec)
        variant-field-predicates (map last variant-spec)]
    `(do
       (defn ~(symbol (str variant-name "?")) [data#]
         (and (= (get data# :type) '~type-name)
              (= (get data# :variant) '~variant-name)))
       (defn ~variant-name [~@variant-args]
         {:pre [~@(map list variant-field-predicates variant-field-names)]}
         {:type '~type-name
          :variant '~variant-name
          :values (array-map
                   ~@(mapcat #(list (keyword %1) %1) variant-field-names))}))))

;; (define-datatype type-name type-predicate-name
;;   { (variant-name { (field-name predicate ) }* ) }+)

(defmacro define-datatype [type-name type-predicate-name & variants]
  `(do
     (data-type-predicate ~type-name ~type-predicate-name)
     ~@(map (fn [v] `(data-type-variant ~v ~type-name)) variants)))

;; (cases type-name expression
;;   {(variant-name ({field-name}*) consequent)}*
;;   (else default))

(defmacro cases [type-name expression & clauses]
  (let [variant (gensym)]
    `(let [~variant ~expression]
       (if (not (= (:type ~variant) '~type-name))
         (throw (Exception. (str "invalid type expected " '~type-name " found '" (:type ~variant) "'"))))
       (cond ~@(mapcat (fn [clause]
                         (let [variant-name (first clause)]
                           (if (= variant-name 'else)
                             `(:else ~@(rest clause))
                             (let [[_ field-names & consequent] clause]
                               `((= (:variant ~variant) '~variant-name)
                                 (apply
                                  (fn [~@field-names]
                                    ~@consequent)
                                  (vals (:values ~variant))))))))
                       clauses)))))

(defn data-value [data field-name]
  (get (get data :values) field-name))
