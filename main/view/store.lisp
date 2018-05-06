(in-package :rdf)

(defparameter *init-state* ())
(defparameter *store-computed* ())
(defparameter *store-actions* ())

(defun add-initial-store-state (key value)
  "Add a key to the initial store state, and store the given PS value in it.
  Store values can be accessed with the {!store} interpolation.
  # Example
  (add-initial-store-state 'count 0)"
  (setf (getf *init-state* key) value))

(defun add-store-computed (name &rest body)
  "Add a computed property to the store. This can be accessed just like normal
  store properties, and the body forms will be executed (body forms should be
  valid parenscript, this will be run on the client)
  The name is the name of the field - if name is 'foo', you can access this
  property with {!store.foo}. If name is 'foo.bar', you can access that property
  with {!store.foo.bar}."
  (setf (getf *store-computed* (intern (string name) :keyword))
        `(lambda () ,@(expand-all-ps-injects
                       (expand-with-symbol-table body '(:{!store} (! (@ window store)))))))
  )

(defun add-store-action (name params &rest body)
  "Add an action to the store. An action is some code which by convention will
  run some code, normally asynchronous (i.e. requesting some data), then update
  the store. For naming rules, see add-store-computed. The body forms should
  return a promise which completes when the action is completed. The body forms
  should be valid parenscript.
  Params is a list of arguments the action will take. These will be in scope for the body forms.

  # Example
  (add-store-action 'fetch-data (page) '(app-req \"/fetch-data\" (array page)))
  This adds a store action called 'fetch-data', which called the '/fetch-data'
  app-req and passes in the given page number. "
  (setf (getf *store-actions* name)
        `(lambda (,@params)
           ,@(expand-all-ps-injects
              (expand-with-symbol-table
               body '(:{!store} (! (@ window store))))))))
