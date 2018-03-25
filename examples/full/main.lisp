(in-package :rdf-full-example)

(defun main () 
  (rdf:rdf-start)
  (rdf:define-app-req "/hello" () (lambda () "Hello, World!")))
