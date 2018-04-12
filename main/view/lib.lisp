(in-package :rdf)

(defvar client-default-unauthorised-behaviour '(alert "Unauthorised"))

(defun set-client-default-unauthorized-behaviour (expr)
  "Customise the parenscript code run when a request gets rejected due to
authorization issues. By default, this simply alerts 'unauthorised'. A good
value for this might be to call 'm.route.set', the mithril function to navigate
to another page, and navigate to a login page."
  (setf client-default-unauthorised-behaviour
        (expand-all-ps-injects
         (expand-with-symbol-table expr '(:{!store} (! (@ window store)))))))

(defpsmacro dispatch-action (action-name &optional params callback error-callback)
  (let ((params-parsed (if (and (listp params) (eq 'array (car params)))
                           params `(array ,params))))
   `(let ((res (chain window store-actions ,action-name (apply null ,params-parsed))))
      ,(if callback `(chain res (then ,callback)))
      ,(if error-callback `(chain res (then ,error-callback)))
      )))

(defun render-lib-js ()
  "Returns a js library with convenience functions"
  (eval
   `(ps (progn
          ,*ps-lisp-library*
          (defun app-req (uri params callback &optional error-callback)
            "Send an app request to the given uri with the given params. params is
       an array of parameters - for example, (array \"john\" \"doe\"). These
       will match up with the app-req (defined with rdf:define-app-req)
       Keywords can also be used for keys, but aren't necessary.

       The callback is a function which is executed when the XHR
       returns. This function should take 2 parameters - a 'body' parameter,
       which contains the returned data, and a 'status' parameter which contains
       the HTTP status code.

       Automatic error handling will be used, writing to {!store.rdf-app-error}
       the returned error message. If you want to override this behaviour, pass
       in an error-callback function.

       See also: rdf:define-app-req function."
            (let ((params-obj (create)))
              (if (instanceof params *Array) 
                  (loop for i from 0 for p in params do (setf (@ params-obj (progn i)) p))
                  (setf (@ params-obj 0) params))
              (let ((req (chain
                          m (request
                             (create
                              method "POST"
                              url uri
                              extract (lambda (res) res)
                              data params-obj)
                             ))))
                (chain
                 req
                 (then
                  (lambda (res)
                    (callback (chain *json* (parse (@ res response-text))) (@ res status)))))
                (chain
                 req
                 (catch
                     (lambda (res)
                       (chain console (log res))
                       (try
                        (var parsed (chain *json* (parse (@ res response-text))))
                        (:catch (e)
                          ;; If there was an error with JSON parsing the error,
                          ;; just set parsed to a generic error message,
                          ;; there's nothing we can do here.
                          (setf parsed (create :error "An unknown error has occurred."))
                          ))
                       (if error-callback
                           (error-callback parsed (@ res status))
                           (progn
                             (if (= (@ res status) 401)
                                 ,client-default-unauthorised-behaviour
                                 (setf (@ window store rdf-app-error) (@ parsed error)))))
                       ))))))
          ))))
