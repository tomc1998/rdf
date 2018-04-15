(in-package :rdf)

(defvar *server-ref* nil)

(defparameter *default-error-component* (defcomp () '(div {!store.rdf-app-error})))
(defparameter *comp-list* ())
(defparameter *error-component* *default-error-component*)
(defparameter *routes* ())
(defparameter *init-state* ())
(defparameter *lass-styles* ())

(defun register-lass (name lass)
  "Register the given LASS (lisp css) rules to the given name. If the name has
already been used, this removes the lass bound to that name. Use clear-lass to
remove a binding. This is to allow easy loading & overriding of various themes."
  (setf (getf *lass-styles* name) lass))

(defun clear-lass (name) (setf (getf *lass-styles* name) nil))

(defun register-component (name fields template)
  "Register a component. name should be a keywords, see defcomp for fields / template docs"
  (if (getf *comp-list* name)
      (setf (getf *comp-list* name) (defcomp fields template))
      (progn
        (push (defcomp fields template) *comp-list*)
        (push name *comp-list*))))

(defun create-routes (routes)
  "Return a parenscript expression for the mithril routes object, given a alist
mapping routes (strings) to component names (keywords)
    # Example routes alist
    (
      (\"/\"         home)
      (\"/login\"    login-page)
      (\"/register\" register-page)
    )"
  `(create ,@(loop for (k v) in routes append (list k v))))

(defun app (routes)
  (let ((ps-expr
         `(ps
            ;; Create store
            (setf (@ window store)
                  (create
                   ,@(append *init-state*
                             (loop for (k v) on *store-computed* by #'cddr
                                append (list (intern (string k)) v)))))
            ;; Add store actions
            (setf (@ window store-actions) (create ,@*store-actions*))
            ;; Mount mithril routes
            (let* (append (root (chain document (get-element-by-id "rdf-content")))
                          (error-flash (chain document (get-element-by-id "rdf-error-flash")))
                          ,@(reverse (loop for (k v) on *comp-list* by #'cddr collect (list k v)))
                          (error-component ,*error-component*))
              (chain m (route root "/" ,(create-routes routes)))
              (chain m (mount error-flash error-component))
              ))))
    (eval ps-expr)))

(defun set-view-routes (routes)
  "Setup view routes. These map strings (URIs) to components. routes is an assoc list.
  # Example
    (set-view-routes ((\"/\" home)
     (\"/about\" about)))
    This sets '/' to route to a component called 'home', and '/about' to route
    to a component call 'about'."
  (setf *routes* routes))

(defun render-app-css ()
  "Render the app's stylesheet"
  (if (not *lass-styles*) (return-from render-app-css ""))
  (reduce (lambda (s0 s1) (concatenate 'string s0 s1))
          (mapcar #'lass:compile-and-write
                  (loop for (k v) on *lass-styles* by #'cddr append v))))

(defun render-app-js ()
  "Given the current state (*routes* and *comp-list*), render an appropriate
app.js file & return it as a string"
  (app *routes*))
