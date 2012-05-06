(ns eopl.core.link-ref)

(def empty-store '())

(def store (atom :undefined))

(defn initialize-store! []
  (reset! store empty-store))

(def reference? number?)

(defn newref [val]
  (let [next-ref (count @store)]
    (swap! store concat (list val))
    next-ref))

(defn de-ref [ref]
  (nth @store ref))

(defn- setref-inner [store ref val]
  (cond (empty? store) (throw (Exception. (str "invalid reference")))
        (zero? ref) (cons val (rest store))
        :else (cons (first store)
                    (setref-inner (rest store) (dec ref) val))))

(defn setref! [ref val]
  (swap! store setref-inner ref val))

