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
  (log-message* :INFO *smtp-server*)
  (log-message* :INFO *smtp-auth*)
  (log-message* :INFO "~a" *smtp-port*)
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
