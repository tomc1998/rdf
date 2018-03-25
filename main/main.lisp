(in-package :rdf)

(defvar *server-ref* nil)
(defun rdf-start ()
  (if (and *server-ref* (hunchentoot:started-p *server-ref*)) nil
      (progn (setf *server-ref* (make-instance 'hunchentoot:easy-acceptor :port 4242))
             (hunchentoot:start *server-ref*))))

(defun rdf-stop () (if (and *server-ref* (hunchentoot:started-p *server-ref*))
                       (hunchentoot:stop *server-ref*) nil))

(defun app-req (uri params callback)
  "Listen for an app request. An 'app request' is a POST request with json
  parameters. These are formatted automatically for transformation from
  JSON to lisp object.

  The first argument, uri, is a string which will have a post request listener
  bound to it - i.e. \"/login\". No URI arguments - use params for this.

  The second argument, params, is a plist defining the parameters. See
  the Params Form section below.

  The third argument, callback, is a function which is called with the given
  parameters. The parameters are not given in a plist, and are instead jsut
  given in the same order which they're specified in the params plist.

  ## Params form

  The params form is a plist. The property is the name to use for the JSON
  object, and is largely unimportant other than for documentation purposes.
  Each value is a symbol or nil:
  - If the parameter is a symbol, it's the name of an entity to parse from the
    JSON data.
  - If the parameter is nil, it's parsed as an arbitrary lisp value - See the
    'allowed arbitrary values' section below

  Two example params objects is as follows:
  (:user user) ;; This is a single param of type 'user' entity
  ;; These are 2 params of arbitrary lisp types, parsed from the 'username' and
  ;; 'password' keys in the input json object.
  (:username nil :password nil) 

  The parameters will be passed into the callback in the same order (not in a plist).

  # Allowed arbitrary values
  Arbitrary lisp values can be sent. Aside from defined entities, the allowed
  types are:
    - number
    - string
    - T / NIL ( interpreted as true / false rather than null. Because of
      javascript's 'falsy' behaviour, nil is essentially equivalent to null. )
    - plist (Serialised as a JSON object)
    - list (Serialised as a JSON array)

  # Returning data
  The value of the callback will be serialised into JSON and returned. The app
  framework will deal with serialisation / deserialisation - either an entity of
  one of the allowed arbitrary types is allowed.

  # Examples
  ;; Passing in username & password
  (app-req \"/login\" (:username nil :password nil)
    (lambda (u p) (if (check-username-password u p) T NIL)))

  ;; Passing in a user-settings entity (whatever that's defined as)
  (app-req \"/update-user-settings\" (:settings 'user-settings)
    (lambda (s) (update-user-settings s)))"
  (hunchentoot:define-easy-handler (uri :uri uri) ()
    (setf (hunchentoot:content-type*) "application/json")
    (funcall callback))
  )
