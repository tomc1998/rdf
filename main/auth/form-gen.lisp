(in-package :rdf)

(defvar *auto-login-form-callback* '(lambda ())
  "When changing this, make sure to call gen-login-form to have your changes
  applied.")
(defvar *auto-reg-form-callback* '(lambda ())
  "When changing this, make sure to call gen-login-form to have your changes
  applied.")

(defun kebab-to-readable (s)
  "Returns a human readable string of the given kebab-case string"
  (let* ((words-raw (str:split "-" s))
         (words (cons (string-capitalize (car words-raw))
                      (mapcar #'string-downcase (cdr words-raw)))))
    (reduce (lambda (s0 s1) (concatenate 'string s0 " " s1)) words)))

(defun gen-login-form ()
  "Generates a login form with gen-form - binds to rdf:auto-login-form. Assumes
  email-pwd auth.
  When changing auto-login-form-callback, call this afterwards."
  (print "GEN LOGIN FORM")
  (gen-form
   'auto-login-form
   '(((email "Email" :required t :type "email" placeholder "Enter your email here"))
     ((pass "Password" :required t :type "password" placeholder "Enter your  here")))
   `(app-req "/rdf/login" obj ,*auto-login-form-callback*)
   ))

(defun gen-reg-form (fields)
  "Generates a reg form with gen-form - binds to rdf:auto-reg-form. Assumes
  email-pwd auth.
  When changing auto-reg-form-callback, call this afterwards."

  (gen-form
   'auto-reg-form
   `(,@(loop for f in fields
          collect (list (list (car f) (kebab-to-readable (string (car f))) :required t)))
       ((email "Email" :required t :type "email" placeholder "Enter your email here"))
       ((pass "Password" :required t :type "password" placeholder "Enter your password here"))
       ((confirm-password "Confirm Password" :required t :type "password" placeholder "Enter your password again"))
       )
   `(app-req "/rdf/register" (array obj obj) ,*auto-reg-form-callback*)
   ))
