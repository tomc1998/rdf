(in-package :rdf)

(defparameter *smtp-server* "localhost" "The address of the SMTP server to use
                                        when sending emails.")
(defparameter *smtp-port* 25 "The port number of the SMTP server")
(defparameter *smtp-auth* () "A list of username & password to use when sending emails.")
(defparameter *email-domain* nil "The email domain used for the 'from' section
                                  of the email. If this isn't defined,
                                  *base-url* is used.")

(defun send-email (addr subject body &key from html-message)
  "Send an email to the given email address."
  (cl-smtp:send-email
   *smtp-server*
   (format nil "~a@~a"
           (if from from "contact")
           (if *email-domain* *email-domain* *base-url*))
   addr
   subject
   body
   :port *smtp-port*
   :html-message html-message
   :authentication *smtp-auth*
   ))

(defun send-email-to-user (id subject body &key from html-message)
  "Send an email to the email address of the given user ID. This fetches the
email from the database first, and assumes an auto-auth system is used, and that
email / password is used as an authentication option."
  (let ((users (select-tree '(user-auth)
                            :where `(= ,id (user-auth parent-user-info-id))))
        )
    (if (= (length users) 0) (error "Trying to send email to user with id ~a,
    but user email was not found. Check the user_auth table!" id))
    (send-email (slot-value (caar users) 'email) subject body
                :from from :html-message html-message)))
