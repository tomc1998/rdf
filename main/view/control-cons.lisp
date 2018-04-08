(in-package :rdf)

(define-condition control-cons-parse-error (error) ((text :initarg :message)))

(defun expand-loop-control (c)
  "Expands a loop control cons. Called from try-expand-control-cons. If c is NOT
  a loop control cons, this is undefined behaviour."
  (if (not (string= (string-downcase (second c)) "for"))
      (error 'control-cons-parse-error :text "Expected 'for' in !loop control"))
  (if (not (string= (string-downcase (fourth c)) "in"))
      (error 'control-cons-parse-error :text "Expected 'in' in !loop control"))
  (let* ((binding (string (third c)))
        ;; Lookup the target of the loop in the symbol list
        (target (fifth c))
        ;; Get the 'rest' of the list
         (body (nthcdr 5 c))
         (symbol-table `(,(intern (format nil "{~a}" binding) :keyword) (! item))))
    `((! ,(loop for b in body append
               (let ((expanded (expand-with-symbol-table b symbol-table)))
                 `(loop for item in ,target collect
                       ,(render (expand-all-control-structures expanded)))
                 ))))))

(defun expand-model-control (c)
  (if (> (length c) 2) (error 'control-cons-parse-error :text
                              (format nil "!model has trailing symbols: ~s" (nthcdr 2 c))))
  (let ((binding (second c)))
    `(onchange
      (! (chain m (with-attr "value" (lambda (v) (setf ,binding v)))))
      value ,binding)))

(defun expand-if-control (c)
  (if (not (or (= (length c) 3) (= (length c) 4)))
      (error 'control-cons-parse-error
             :text (format nil "!if control has wrong number of symbols: ~a - should be 3 or 4"
                           (length c))))
  (let ((condition (second c))
        (if-value (third c)))
    (if (= (length c) 4)
        `((! (if ,condition ,(render (expand-all-control-structures if-value)) ,(render (fourth c)))))
        `((! (if ,condition ,(render (expand-all-control-structures if-value))))))))

(defun expand-class-control (c)
  (let* ((try-expand-conditional-class
         (lambda (c)
           (if (listp c)
               (progn
                 (if (not (string= (string-downcase (car c)) "!if"))
                     (error 'control-cons-parse-error
                            :text
                            (format nil "Unsupported cons inside !class: ~s
                        - should start with !if" c)))
                 (car (expand-if-control c))) c)))
         (bindings (cons 'array (mapcar try-expand-conditional-class (cdr c)))))
    `((! (reduce (lambda (s0 s1)
                   (concatenate 'string s0 " " s1)
                   ) ,bindings)))))

(defun try-expand-control-cons (c)
  "A control cons is a cons who's car begins with a special template symbol, one
    that begins with a ! (i.e. !loop or !if) - this function will return either nil
    (if c is NOT a valid control cons), or a valid parenscript form to generate the
    expanded template. This can be interpolated straight into the template list.
    A control-cons-parse-error will be raised if c IS a control cons but in the
    wrong format.

    # Looping (arrays)
        ;; List of links (a href)
        (!loop for x in {array-attrib} (li ((a href {x}) {x})))"
  ;; Check if c actually is a control cons, return otherwise
  (if (not (typep c 'cons)) (return-from try-expand-control-cons nil))
  (if (not (typep (car c) 'symbol)) (return-from try-expand-control-cons nil))
  (if (not (char= #\! (char (string (car c)) 0))) (return-from try-expand-control-cons nil))
  (cond
    ((string= (string-downcase (car c)) "!loop") (expand-loop-control c))
    ((string= (string-downcase (car c)) "!model") (expand-model-control c))
    ((string= (string-downcase (car c)) "!if") (expand-if-control c))
    ((string= (string-downcase (car c)) "!class") (expand-class-control c))
    (t nil)))

(defun expand-all-control-structures (template)
  "Returns the template with all control structures (i.e. !loop) expanded."
  (cond
    ((listp template)
     (loop for i from 0 for item in template append
          (let ((expanded (try-expand-control-cons item)))
            (if expanded expanded
                ;; If we failed to expand, just treat this as a normal list
                (list (expand-all-control-structures item))))))
    (t template)))

