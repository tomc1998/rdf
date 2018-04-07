(in-package :rdf)

(defvar *server-ref* nil)
(defvar *verify-auth* (lambda (type) (declare (ignore type))
                              (error 'error "No *verify-auth* function defined")))
(defvar *additional-stylesheet-urls* ())
(defvar *additional-script-urls* ())

(defun add-additional-stylesheet-url (url) (push url *additional-stylesheet-urls*))
(defun clear-additional-stylesheets () (setf *additional-stylesheet-urls* ()))
(defun add-additional-script-url (url) (push url *additional-script-urls*))
(defun clear-additional-scripts () (setf *additional-script-urls* ()))


;; Redefine functions so we can re-export with nicer names
(setf (fdefinition 'hash-pwd) #'ironclad:pbkdf2-hash-password-to-combined-string)
(setf (fdefinition 'check-pwd) #'ironclad:pbkdf2-check-password)
(setf (fdefinition 'string-to-octets) #'flexi-streams:string-to-octets)

(defun setup-view-routes ()
  (hunchentoot:define-easy-handler (index :uri "/" :default-request-type :GET) ()
    (setf (hunchentoot:content-type*) "text/html")
    (format nil "<html>
      <head>
        <link rel=\"stylesheet\" href=\"/rdf/style.css\" type=\"text/css\">
        ~{
          <link rel=\"stylesheet\" href=\"~a\" type=\"text/css\">
        ~}
      </head>
      <body>
        <div id=\"rdf-content\"></div>
        <div id=\"rdf-error-flash\"></div>
        ~{
        <script src=\"~a\"></script>
        ~}
        <script src=\"//unpkg.com/mithril/mithril.js\"></script>
        <script src=\"/rdf/lib.js\"></script>
        <script src=\"/rdf/app.js\"></script>
      </body>
    </html>" *additional-stylesheet-urls* *additional-script-urls*))
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
  ;; Set return codes & error response
  (hunchentoot:log-message* :ERROR "~A: ~A | CODE: ~A" (write condition :escape nil)
                            (slot-value condition 'message)
                            (slot-value condition 'code))
  (setf (hunchentoot:return-code*) (slot-value condition 'code))
  (list :error (slot-value condition 'message)))

(defun raise-app-error (&optional (message "An error has occurred.") (code 500))
  " A convenience function to raise an error. Pass in a string message to pass
back to the client, and an error code type if you're doing some custom
handling."
  (error 'app-req-error :message message :code code))

(defmacro define-app-req (uri params callback &key require-auth)
  "Listen for an app request. An 'app request' is a POST request with json
  parameters. These are formatted automatically for transformation from
  JSON to lisp object.

  The first argument, uri, is a string which will have a post request listener
  bound to it - i.e. \"/login\". No URI arguments - use params for this.

  The second argument, params, is a listdefining the parameters. See
  the Params Form section below.

  The third form is the callback called when a request comes through. It must
  accept the same amount of arguments as specified in the params form.

  The 'require-auth' key parameter is by default nil - if set to non-null, this
  endpoint will need to be verified using the *verify-auth* function. The value of
  the require-auth key parameter will be passed to the *verify-auth* function.
  If this *verify-auth* function returns T, the request proceeds as normal - if
  it returns false, the request is rejected and a 401 error is returned. See
  `set-client-default-unauthorized-behaviour` for customising how the client
  reacts. Different values can be used for different levels of authentication -
  for example, :require-auth 'normal and :require-auth 'admin.

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
       ;; If this is an authenticated endpoint, check auth
       (to-json
        (handler-case
            (progn
              ,(if require-auth `(if (not (funcall *verify-auth* ,require-auth))
                                     (error 'app-req-error :message "Unauthorized." :code 401)))
              (let ((data (from-json (string (hunchentoot:raw-post-data :force-text t)))))
                (if (not (typep data 'list)) (error 'error "Error - app req param is not an object"))
                (if (not (is-plist data)) (error 'error "Error - app req param is not an object"))
                ;; Get the params from the data
                (let ((params-parsed
                       (loop for i from 0 to ,(1-(length params))
                          for p in ',params collect
                            (let ((val (getf data (intern (write-to-string i) :keyword))))
                              (if p (entity-from-json p val) val)))))
                  (apply callback params-parsed))))
          (app-req-error (condition) (handle-app-req-error condition)))))))
