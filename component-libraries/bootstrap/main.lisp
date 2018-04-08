(in-package :bs)

(defun register-container-row-column ()
  (rdf:register-component
   'bs-container '(:attrs (fluid class))
   '((div class (!class {class} (!if {fluid} "container-fluid" "container")))
     {children}))
  (rdf:register-component
   'bs-row '(:attrs (class)) '((div class (!class "row" {class})) {children}))
  (rdf:register-component
   'bs-col '(:attrs
            (;; Array of strings like (array 'sm-6' 'lg-4') for small, 6 columns,
             ;; large, 4 columns. See bootstrap docs.
             types
             class)
            :computed
            ((full-bs-class-name
              (if {types}
                  (reduce (lambda (s0 s1) (+ s0 " " s1))
                          (mapcar (lambda (s) (+ "col-" s)) {types}))
                  "col"))))
   '((div class (!class {full-bs-class-name} {class})) {children})))

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

  ;; Load bootstrap components
  (register-container-row-column)
  )
