(in-package :bs)

(defun load-all ()
  (print "Loading bootstrap components...")
  ;; Add bootstrap CSS
  (rdf:add-additional-stylesheet-url
   "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css")

  ;; Add bootstrap script dependencies - JQuery, Popper, and Bootstrap
  (rdf:add-additional-script-url
   "https://code.jquery.com/jquery-3.2.1.slim.min.js")
  (rdf:add-additional-script-url
   "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js")
  (rdf:add-additional-script-url
   "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js")

  )