;; Add to the language of this section a construct called
;; yield. Whenever a thread executes a yield, it is placed on the
;; ready queue, and the thread at the head of the ready queue is
;; run. When the thread is resumed, it should appear as if the call to
;; yield had returned the number 99.

;; see eopl.core.thread-lang
