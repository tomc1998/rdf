(in-package :rdf)

(defun kebab-to-readable (s)
  "Returns a human readable string of the given kebab-case string"
  (let* ((words-raw (str:split "-" s))
         (words (cons (string-capitalize (car words-raw))
                      (mapcar #'string-downcase (cdr words-raw)))))
    (reduce (lambda (s0 s1) (concatenate 'string s0 " " s1)) words)))

(defun gen-login-form (fields)
  "Generates a login form with gen-form - binds to rdf:auto-login-form. Assumes
  email-pwd auth."
  (gen-form
   'auto-login-form
   `(,@(loop for f in fields
          collect (list (list (car f) (kebab-to-readable (string (car f))))))
       ((email "Email" :type "email" placeholder "Enter your email here"))
       ((password "Password" :type "password" placeholder "Enter your  here"))
       )))

(defun gen-reg-form (fields)
  "Generates a login form with gen-form - binds to rdf:auto-login-form. Assumes
  email-pwd auth."
  (gen-form
   'auto-reg-form
   `(,@(loop for f in fields
          collect (list (list (car f) (kebab-to-readable (string (car f))))))
       ((email "Email" :type "email" placeholder "Enter your email here"))
       ((password "Password" :type "password" placeholder "Enter your password here"))
       ((confirm-password "Confirm Password" :type "password" placeholder "Enter your password again"))
       )))
