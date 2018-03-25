(in-package :rdf)

(rdf-stop)

;; Test start / stop
(prove:plan 4)
(prove:ok (rdf-start))
(prove:ok (not (rdf-start)))
(prove:ok (rdf-stop))
(prove:ok (not (rdf-stop)))
(prove:finalize)

