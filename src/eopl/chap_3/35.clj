;; The representation we have seen so far are inefficient, because
;; they build a new closure every time the procedure is retrieved. But
;; the closure is the same every time. We can build the closures only
;; once, by putting the value in a vector of length 1 and building an
;; explicit circular structure, like

;; see eopl.core.env/extend-env-rec
