(in-package :rdf)

(defun transform-decoded-json (j)
  "After decoding json with cl-json, this will transform it into lisp objects
  which are compatible with to-json (with knowledge of registered entities etc)"

  ;; Check if j is object
  (if (typep j 'list) (if (and (typep (car j) 'list) (typep (caar j) 'keyword))
             ;; Is object
             (loop for (k . v) in j append `(,k ,(transform-decoded-json v)))
             ;; Is array
             (loop for i in j collect (transform-decoded-json i)))
      j))

(defun from-json (s)
  "Parse a json string into a lisp object. This is a wrapper on top of cl-json
  which has awareness for parsing into / out of entities."
  (let ((decoded (cl-json:decode-json-from-string s)))
    (transform-decoded-json decoded)
    ))
