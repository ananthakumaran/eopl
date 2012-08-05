(ns eopl.core.scheduler
  (:use eopl.core.define-datatype)
  (:use eopl.core.vector-ref))

(def ready-queue (atom :undefined))
(def final-answer (atom :undefined))
(def max-time-slice (atom :undefined))
(def time-remaining (atom :undefined))

(defn initialize-scheduler! [ticks]
  (reset! ready-queue '())
  (reset! final-answer :uninitialized)
  (reset! max-time-slice ticks)
  (reset! time-remaining @max-time-slice))

(defn place-on-ready-queue! [thread]
  (swap! ready-queue concat (list thread)))

(defn run-next-thread []
  (if (empty? @ready-queue)
    @final-answer
    (let [[first-ready-thread & other-ready-threads] @ready-queue]
      (reset! ready-queue other-ready-threads)
      (reset! time-remaining @max-time-slice)
      (first-ready-thread))))

(defn set-final-answer! [val]
  (reset! final-answer val))

(defn time-expired? []
  (zero? @time-remaining))

(defn decrement-timer! []
  (swap! time-remaining dec))


(defn wait-for-mutex [mut thread]
  (cases mutex mut
    (a-mutex (ref-to-closed? ref-to-wait-queue)
             (if (de-ref ref-to-closed?)
               (do
                 (setref! ref-to-wait-queue
                          (concat (de-ref ref-to-wait-queue) (list thread)))
                 (run-next-thread))
               (do
                 (setref! ref-to-closed? true)
                 (thread))))))

(defn signal-mutex [mut thread]
  (cases mutex mut
    (a-mutex (ref-to-closed? ref-to-wait-queue)
             (let [closed? (de-ref ref-to-closed?)
                   queue (de-ref ref-to-wait-queue)]
               (if closed?
                 (if (empty? queue)
                   (do
                     (setref! ref-to-closed? false))
                   (let [[first & rest] queue]
                     (place-on-ready-queue! first)
                     (setref! ref-to-wait-queue rest)))))
             (thread))))
