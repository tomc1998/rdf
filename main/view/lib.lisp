(in-package :rdf)

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

       Automatic error handling will be used, writing to {$store.rdf-app-error}
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
                       (if error-callback
                           (error-callback (chain *json* (parse (@ res response-text))) (@ res status))
                           (setf (@ window store rdf-app-error) (chain *json* (parse (@ res response-text )) error)))
                       ))))))))))
