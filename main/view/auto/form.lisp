(in-package :rdf)
(defparameter *gen-form-func*
  (lambda (name input-groups &rest callback)
    (eval
     `(rdf:register-component
       ',name
       '(:state ((obj (create ,@(loop for i-group in input-groups append
                                     (loop for i in i-group append (list (car i) ""))))))
         :methods ((onsubmit () (progn (var obj {obj}) ,@callback))))
       '((form onsubmit {@onsubmit})
         ,@(loop for i-group in input-groups collect
                `(fieldset
                  ,@(loop for (name label . options) in i-group do
                       ;; Error handle just for convenience
                         (if (not name) (error "No input name in gen-form"))
                         (if (not label) (error "No input label in gen-form"))
                         (if (oddp (length options))
                             (error "gen-form input options should be plist (length is odd)"))
                       append
                         (let ((attribs (loop for (k v) on options by #'cddr append
                                             (list (intern (string k)) v)))
                               (model (intern (format nil "{OBJ.~a}" (string-upcase name)))))
                           `(,(if label `(!array ((label for ,(string name)) ,label) (br)))
                              ((input ,@attribs id ,(string name) (!model ,model)))
                              (br))))))
         ((button class "btn btn-primary" type "submit") "Submit")))))
  )

(defun gen-form (name input-groups &rest callback)
  "Registers a component with the given symbol, given the input groups.
  input-groups is a list, where each item is a list containing an input. An input is a list of the following format:

  (id label . options)

  The ID is the unique name of the field. When the form is filled in, this data
  will be extracted into a variable of this name.
  Label is the text used to label the input.
  Options is a plist of input attributes. Here is a full input example:
  (email \"Email\" :type \"text\" :placeholder \"Please enter your email here\")
  Options can also be used for the html5 form validation:
  (age \"Age\" :type \"number\" :required t :max 130)

  The callback forms are a list of parenscript expressions which are run when
  the form is submitted. The 'obj' variable is in scope, and contains the inputs
  of the form.

  # Example
  (rdf:gen-form
   'login-form
   '(((first-name \"First name\")
      (last-name \"Last name\"))
     ((email \"Email\" :placeholder \"Enter your email here\" :type \"email\"))
     ((password \"Password\" :placeholder \"Enter your password here\" :type \"password\")))
   '(chain console (log obj)))

  This example generates a form which takes a first name, last name, email, and
  password. When the form is submitted, it called console.log(obj), where 'obj'
  is the object containing all of the inputs. The keys of the object correspond
  to the names of the inputs, for example the first name input will be at
  obj.firstName, which can be accessed as (@ obj first-name) in parenscript."
  (apply *gen-form-func* (append (list name input-groups) callback)))
