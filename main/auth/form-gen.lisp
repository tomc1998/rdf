(in-package :rdf)

(defvar *auto-login-form-callback* '(lambda ()))
(defvar *auto-reg-form-callback* '(lambda ()))

(defun kebab-to-readable (s)
  "Returns a human readable string of the given kebab-case string"
  (let* ((words-raw (str:split "-" s))
         (words (cons (string-capitalize (car words-raw))
                      (mapcar #'string-downcase (cdr words-raw)))))
    (reduce (lambda (s0 s1) (concatenate 'string s0 " " s1)) words)))

(defun gen-login-form ()
  "Generates a login form with gen-form - binds to rdf:auto-login-form. Assumes
  email-pwd auth."
  (gen-form
   'auto-login-form
   `(((email "Email" :required t :type "email" placeholder "Enter your email here"))
     ((password "Password" :required t :type "password" placeholder "Enter your  here")))
   `(app-req "/rdf/login" obj ,*auto-login-form-callback*)
   ))

(defun gen-reg-form (fields)
  "Generates a login form with gen-form - binds to rdf:auto-login-form. Assumes
  email-pwd auth."
  (gen-form
   'auto-reg-form
   `(,@(loop for f in fields
          collect (list (list (car f) (kebab-to-readable (string (car f))) :required t)))
       ((email "Email" :required t :type "email" placeholder "Enter your email here"))
       ((password "Password" :required t :type "password" placeholder "Enter your password here"))
       ((confirm-password "Confirm Password" :required t :type "password" placeholder "Enter your password again"))
       )
   `(app-req "/rdf/register" obj ,*auto-reg-form-callback*)
   ))
