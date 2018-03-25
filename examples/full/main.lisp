(in-package :rdf-full-example)

(defun main () 
  (rdf:rdf-start)
  (rdf:app-req "/hello" () (lambda () "Hello")))
