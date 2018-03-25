;; Test start / stop
(prove:plan 4)
(prove:ok (rdf:rdf-start))
(prove:ok (not (rdf:rdf-start)))
(prove:ok (rdf:rdf-stop))
(prove:ok (not (rdf:rdf-stop)))
(prove:finalize)
