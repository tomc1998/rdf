(in-package :rdf)

(defun is-plist (l)
  "Given a list, checks if every even element is a keyword, and if the list has
    an even number of elements. If so returns T, otherwise, NIL"
  (if (evenp (length l))
      (let ((is-plist T))
        (block loop
          (loop for x in l for i from 0 when (evenp i) do
               (if (not (typep x 'keyword))
                   (progn (setf is-plist NIL) (return-from loop)))))
        is-plist)
      nil))

(defun kebab-to-camel-case (s)
  "Given a string s, convert that string to camel case"
  (let ((splits (str:split "-" s)))
    (apply 'concatenate (append (list 'string (car splits))
                                (loop for word in (cdr splits)
                                   collect (string-capitalize word))))))

(define-condition malformed-to-json-input (error) ((text :initarg :text)))

(defun to-json (o)
  "Serialise the lisp object 'o' to JSON. Supported types:
    - number
    - string
    - T / NIL ( interpreted as true / false rather than null. Because of
      javascript's 'falsy' behaviour, nil is essentially equivalent to null. )
    - plist (Serialised as a JSON object)
    - list (Serialised as a JSON array)"

  (cond
    ((typep o 'string) (format nil "\"~a\"" o))
    ((typep o 'number) (format nil "~a" o))
    ((typep o 'cons)
     (if (is-plist o)
         (format nil "{~{~a~^,~}}"
                 ;; Collect all key / value pairs, convert value to json
                 (loop for (k v) on o by #'cddr
                      collect (format nil "\"~a\":~a" (kebab-to-camel-case (string-downcase k)) (to-json v))))
         (format nil "[~{~a~^,~}]" (loop for x in o collect (to-json x)))))
    ;; serialise lisp object
    ((typep o 'standard-object) (to-json (entity-to-json o)))
    ((eq o t) "true")
    ((eq o nil) "false")
    (t (error 'malformed-to-json-input :text (format nil "Cannot convert ~a to JSON" o)))))
