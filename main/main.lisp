(in-package :rdf)

(defvar *server-ref* nil)
(defun rdf-start ()
  (if (and *server-ref* (hunchentoot:started-p *server-ref*)) nil
      (progn (setf *server-ref* (make-instance 'hunchentoot:easy-acceptor :port 4242))
             (hunchentoot:start *server-ref*))))

(defun rdf-stop () (if (and *server-ref* (hunchentoot:started-p *server-ref*))
                       (hunchentoot:stop *server-ref*) nil))
