(in-package :rdf)

(defvar *server-ref* nil)

(setf (fdefinition 'hash-pwd) #'ironclad:pbkdf2-hash-password-to-combined-string)
(setf (fdefinition 'check-pwd) #'ironclad:pbkdf2-check-password)
(setf (fdefinition 'string-to-octets) #'flexi-streams:string-to-octets)

(defun setup-view-routes ()
  (hunchentoot:define-easy-handler (index :uri "/" :default-request-type :GET) ()
    (setf (hunchentoot:content-type*) "text/html")
    "<html>
      <head>
        <link rel=\"stylesheet\" href=\"/rdf/style.css\" type=\"text/css\">
      </head>
      <body>
        <div id=\"rdf-content\"></div>
        <div id=\"rdf-error-flash\"></div>
        <script src=\"//unpkg.com/mithril/mithril.js\"></script>
        <script src=\"/rdf/lib.js\"></script>
        <script src=\"/rdf/app.js\"></script>
      </body>
    </html>")
  (hunchentoot:define-easy-handler (app-css :uri "/rdf/style.css" :default-request-type :GET) ()
    (setf (hunchentoot:content-type*) "text/css")
    (render-app-css))
  (hunchentoot:define-easy-handler (app-js :uri "/rdf/app.js" :default-request-type :GET) ()
    (setf (hunchentoot:content-type*) "application/javascript")
    (render-app-js))
  (hunchentoot:define-easy-handler (lib-js :uri "/rdf/lib.js" :default-request-type :GET) ()
    (setf (hunchentoot:content-type*) "application/javascript")
    (render-lib-js))
  )

(defun rdf-start ()
  (setup-view-routes)
  (if (and *server-ref* (hunchentoot:started-p *server-ref*)) nil
      (progn (setf *server-ref* (make-instance 'hunchentoot:easy-acceptor :port 4242))
             (hunchentoot:start *server-ref*))))

(defun rdf-stop () (if (and *server-ref* (hunchentoot:started-p *server-ref*))
                       (hunchentoot:stop *server-ref*) nil))

(define-condition app-req-error (error) ((message :initarg :message
                                                  :initform "An error has occurred.")
                                         (code :initarg :code :initform 500)))

(defun handle-app-req-error (condition)
  "Called from the define-app-req handler-case"
  ;; Log the error
  (hunchentoot:log-message*
   :ERROR "An error has occurred: ~s."
   (write (slot-value condition 'message) :escape nil))
  ;; Set return codes & error response
  (setf (hunchentoot:return-code*) (slot-value condition 'code))
  (list :error (slot-value condition 'message)))

(defun raise-app-error (&optional (message "An error has occurred.") (code 500))
  " A convenience function to raise an error. Pass in a string message to pass
back to the client, and an error code type if you're doing some custom
handling."
  (error 'app-req-error :message message :code code))

(defmacro define-app-req (uri params callback)
  "Listen for an app request. An 'app request' is a POST request with json
  parameters. These are formatted automatically for transformation from
  JSON to lisp object.

  The first argument, uri, is a string which will have a post request listener
  bound to it - i.e. \"/login\". No URI arguments - use params for this.

  The second argument, params, is a listdefining the parameters. See
  the Params Form section below.

  The third form is the callback called when a request comes through. It must
  accept the same amount of arguments as specified in the params form.

  ## Params form

  The params form is a list of arguments to accept
  Each value is a symbol or nil:
  - If the parameter is a symbol, it's the name of an entity to parse from the
    JSON data.
  - If the parameter is nil, it's parsed as an arbitrary lisp value - See the
    'allowed arbitrary values' section below

  Two example params objects is as follows:
  (user) ;; This is a single param of type 'user' entity
  (nil nil) ;; These are 2 params of arbitrary lisp types.

  # Allowed arbitrary values
  Arbitrary lisp values can be sent. Aside from defined entities, the allowed
  types are:
    - number
    - string
    - T / NIL ( interpreted as true / false rather than null. Because of
      javascript's 'falsy' behaviour, nil is essentially equivalent to null. )
    - plist (Serialised as a JSON object)
    - list (Serialised as a JSON array)

  The to-json function is used for json ser.

  # Returning data
  The value of the body forms will be serialised into JSON and returned. The app
  framework will deal with serialisation / deserialisation - either an entity of
  one of the allowed arbitrary types is allowed.

  # Examples
  ;; Passing in username & password
  (app-req \"/login\" '(nil nil)
    (lambda (username password)
      (if (check-username-password username password) T NIL)))

  ;; Pass through user-settings entity to the update-user-settings function
  ;; (which should take 1 parameter)
  (app-req \"/update-user-settings\" '(user-settings) update-user-settings)"
  `(let ((callback ,callback))
     (hunchentoot:define-easy-handler (,(intern uri) :uri ,uri :default-request-type :POST) ()
       (setf (hunchentoot:content-type*) "application/json")
       (let ((data (from-json (string (hunchentoot:raw-post-data :force-text t)))))
         (if (not (typep data 'list)) (error 'error "Error - app req param is not an object"))
         (if (not (is-plist data)) (error 'error "Error - app req param is not an object"))
         ;; Get the params from the data
         (let ((params-parsed
                (loop for i from 0 to ,(1-(length params))
                   for p in ',params collect
                     (let ((val (getf data (intern (write-to-string i) :keyword))))
                       (if p (entity-from-json p val) val)))))
           (to-json (handler-case
                        (apply callback params-parsed)
                      (app-req-error (condition) (handle-app-req-error condition))
                      (error () (handle-app-req-error (make-instance 'app-req-error))))))))))
