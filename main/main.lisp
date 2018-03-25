(in-package :rdf)

(defvar *server-ref* nil)
(defun rdf-start ()
  (if *server-ref* (if (hunchentoot:started-p *server-ref*) (return-from rdf-start)))
  (setf *server-ref* (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (hunchentoot:start *server-ref*))

(defun rdf-stop () (if *server-ref* (if (hunchentoot:started-p *server-ref*)
                                        (hunchentoot:stop *server-ref*))))
