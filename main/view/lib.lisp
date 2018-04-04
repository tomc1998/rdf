(in-package :rdf)

(defun render-lib-js ()
  "Returns a js library with convenience functions"
  (eval
   `(ps (progn
          (defun app-req (uri params callback)
            "Send an app request to the given uri with the given params. params is
       an array of parameters - for example, (array \"john\" \"doe\"). These
       will match up with the app-req (defined with rdf:define-app-req)
       Keywords can also be used for keys, but aren't necessary.

       The callback is a function which is executed when the XHR
       returns. This function should take a 'res' parameter, which contains the returned data.

       See also: rdf:define-app-req function."
            (let ((params-obj (create)))
              (loop for i from 0 for p in params do (setf (@ params-obj (progn i)) p))
              (chain
               m (request
                  (create
                   method "POST"
                   url uri
                   data params-obj)
                  ) (then callback))))))))
