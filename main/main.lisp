(in-package :rdf)

(defvar *server-ref* nil)
(defvar *verify-auth* (lambda (type) (declare (ignore type))
                              (rdf:session-value 'user-id)))
(defvar *additional-stylesheet-urls* ())
(defvar *additional-script-urls* ())

(defun add-additional-stylesheet-url (url) (push url *additional-stylesheet-urls*))
(defun clear-additional-stylesheets () (setf *additional-stylesheet-urls* ()))
(defun add-additional-script-url (url) (push url *additional-script-urls*))
(defun clear-additional-scripts () (setf *additional-script-urls* ()))


;; Redefine functions so we can re-export with nicer names
(defun hash-pwd (pwd)
  "Hash the given string pwd, returning the combined salt digest string. Produces a string of length 116."
  (ironclad:pbkdf2-hash-password-to-combined-string (flexi-streams:string-to-octets pwd)))
(defun check-pwd (pwd combined-salt-and-digest)
  "Check that the string pwd equals the given combined salt and digest string"
  (ironclad:pbkdf2-check-password (flexi-streams:string-to-octets pwd) combined-salt-and-digest))
(setf (fdefinition 'string-to-octets) #'flexi-streams:string-to-octets)

(defun setup-view-routes (&key (static-folder-path "static/"))
  (hunchentoot:define-easy-handler (index :uri "/" :default-request-type :GET) ()
    (setf (hunchentoot:content-type*) "text/html")
    (format nil "<html>
      <head>
        ~{
          <link rel=\"stylesheet\" href=\"~a\" type=\"text/css\">
        ~}
        <link rel=\"stylesheet\" href=\"/rdf/lib.css\" type=\"text/css\">
        <link rel=\"stylesheet\" href=\"/rdf/style.css\" type=\"text/css\">
      </head>
      <body>
        <div id=\"rdf-content\"></div>
        <div id=\"rdf-error-flash\"></div>
        ~{
        <script src=\"~a\"></script>
        ~}
        <script src=\"/rdf/mithril.js\"></script>
        <script src=\"/rdf/lib.js\"></script>
        <script src=\"/rdf/app.js\"></script>
      </body>
    </html>" (reverse *additional-stylesheet-urls*) (reverse *additional-script-urls*)))
  (hunchentoot:define-easy-handler (app-css :uri "/rdf/style.css" :default-request-type :GET) ()
    (setf (hunchentoot:content-type*) "text/css")
    (render-app-css))
  (hunchentoot:define-easy-handler (lib-css :uri "/rdf/lib.css" :default-request-type :GET) ()
    (setf (hunchentoot:content-type*) "text/css")
    (render-lib-css))
  (hunchentoot:define-easy-handler (app-js :uri "/rdf/app.js" :default-request-type :GET) ()
    (setf (hunchentoot:content-type*) "application/javascript")
    (render-app-js))
  (hunchentoot:define-easy-handler (lib-js :uri "/rdf/lib.js" :default-request-type :GET) ()
    (setf (hunchentoot:content-type*) "application/javascript")
    (render-lib-js))
  (define-file-handler "/rdf/mithril.js"
      (asdf:system-relative-pathname :rdf "lib/mithril.js"))
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" static-folder-path) hunchentoot:*dispatch-table*)
  )

(defun rdf-start (&key
                    (port 4242)
                    (static-folder-path "static/")
                    (base-url "localhost:4242"))
  "Start the server.
  # Params
    * port - The port to start the server on. Defaults to 4242.
    * base-url - This is the base URL. If this server is behind a domain name,
      this should be a domain name. This will be used when generating links to
      this server - for example, when generating a one time use link for a
      verification email. Defaults to localhost:4242. Shouldn't end with a '/'.
    * static-folder-path - The path to serve static files from. Defaults to
      'static/'. Consider using asdf:system-relative-pathname.
  "
  (setf *base-url* base-url)
  (setf *static-folder-path* static-folder-path)
  (setup-view-routes :static-folder-path static-folder-path)
  (if (and *server-ref* (hunchentoot:started-p *server-ref*)) nil
      (progn (setf *server-ref* (make-instance 'hunchentoot:easy-acceptor :port port))
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

(defun define-file-handler (uri path)
  "Setup a file handler on a given URI to handle a request & serve the file at
  the given path."
  (print uri)
  (print path)
  (let ((callback (hunchentoot:create-static-file-dispatcher-and-handler uri path)))
    (eval `(hunchentoot:define-easy-handler (,(intern uri) :uri ,uri) ()
             (funcall (funcall ,callback hunchentoot:*request*))))))

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
  - If the parameter is a list, the first and only element of the list is the
    name of an entity. This signifies an array of this entity in the json data.
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
