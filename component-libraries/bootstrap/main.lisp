(in-package :bs)

(defun gen-form (name inputs &rest callback)
  "Registers a bootstrap form with the given inputs. When the form is
  submitted, run the given parenscript forms (onsubmit). The forms are run
  with an object called 'obj' in scope, where all the fields of the object are
  the names of the inputs (see below)
  The first parameter is the name of the generated component. This should be a
  symbol.
  # Input format
  Inputs are a list of lists, each list is of the following format:
  (<name> <label> <placeholder> :type <type>)
  where 'name' is the name of the input, 'label' and 'placeholder' are the label
  and placeholder of the input in the template, and 'type' is an optional field
  type - for example, \"password\". These are all strings.

  Default input type is \"text\".

  For example:
  (gen-form 'my-form '((\"user\" \"Username\" \"Enter your username here\")
            (\"pass\" \"Password\" \"Enter your password here\" :type \"password\"))
    '(app-req \"/login\" obj (lambda (res) (if res (alert \"Logged in.\")
                                                      (alert \"Login failed.\")))))
  This will generate a form which will pass the object {user: ... pass: ...} to
  the URI '/login', then alert 'Logged in.' if the request is successful. "
  (eval `(rdf:register-component
          ',name
          '(:state (obj (create email "" pass ""))
            :methods ((onsubmit () (progn (var obj {obj}) ,@callback))))
          '((form onsubmit {@onsubmit})
           ,@(loop for (name label placeholder . options) in inputs do
                ;; Error handle just for convenience
                  (if (not name) (error 'error "No input name in bs:gen-form"))
                  (if (oddp (length options))
                      (error 'error "bs:gen-form input options should be plist (length is odd)"))
                collect
                  (let ((type (getf options :type))
                        (model (intern (format nil "{OBJ.~a}" (string-upcase name)))))
                    `((div class "form-group")
                      ,(if label `((label for ,name) ,label))
                      ((input type ,(if type type "text") class "form-control" id ,name
                              placeholder ,(if placeholder placeholder "") (!model ,model)))
                      )))
            ((button class "btn btn-primary" type "submit") "Submit")))))

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

  ;; Load error component
  (rdf:register-component
   'bs-error-component
   '(:methods ((onclick () (setf {!store.rdf-app-error} nil))))
   `(div
     (!if
      {!store.rdf-app-error}
      ((div class "container")
       ((div class "row mt-1 justify-content-center"
             style (create top "5px" left "5px" right "5px"
                     position "fixed" z-index "9999"))
        ((div class "col-sm-12 col-md-9 col-lg-6")
         ((div class (!class "alert alert-danger"
                             (!if {!store.rdf-app-error} "show" "hide")) role "alert")
          {!store.rdf-app-error}
          ((button class "ml-2 close" onclick {@onclick}) ,(code-char #x00d7)))))))))
  (setf rdf:*error-component* 'bs-error-component)
  )
