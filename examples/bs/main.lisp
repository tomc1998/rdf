(in-package :rdf-bs-example)

(defun main ()
  ;; Make sure we don't have any additional stylesheets loaded already
  (rdf:clear-additional-stylesheets)
  ;; Load bootstrap components
  (bs:load-all)

  ;; Start the server
  (rdf:rdf-stop)
  (rdf:rdf-start)
  )
