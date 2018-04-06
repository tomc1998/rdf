(in-package :rdf)

(defvar *server-ref* nil)

(define-condition control-cons-parse-error (error) ((text :initarg :message)))

(defun expand-loop-control (c)
  "Expands a loop control cons. Called from try-expand-control-cons. If c is NOT
  a loop control cons, this is undefined behaviour."
  (if (not (string= (string-downcase (second c)) "for"))
      (error 'control-cons-parse-error :text "Expected 'for' in $loop control"))
  (if (not (string= (string-downcase (fourth c)) "in"))
      (error 'control-cons-parse-error :text "Expected 'in' in $loop control"))
  (let* ((binding (string (third c)))
        ;; Lookup the target of the loop in the symbol list
        (target (fifth c))
        ;; Get the 'rest' of the list
         (body (cdr (cddddr c)))
         (symbol-table `(,(intern (format nil "{~a}" binding) :keyword) ($ item))))
    `(($ ,(loop for b in body append
               (let ((expanded (expand-with-symbol-table b symbol-table)))
                 `(loop for item in ,target collect
                       ,(render expanded))
                 ))))))

(defun expand-model-control (c)
  (let ((binding (second c)))
    `(onchange
      ($ (chain m (with-attr "value" (lambda (v) (setf ,binding v)))))
      value ,binding)))

(defun try-expand-control-cons (c)
  "A control cons is a cons who's car begins with a special template symbol, one
    that begins with a $ (i.e. $loop or $if) - this function will return either nil
    (if c is NOT a valid control cons), or a valid parenscript form to generate the
    expanded template. This can be interpolated straight into the template list.
    A control-cons-parse-error will be raised if c IS a control cons but in the
    wrong format.

    # Looping (arrays)
        ;; List of links (a href)
        ($loop for x in {array-attrib} (li ((a href {x}) {x})))"
  ;; Check if c actually is a control cons, return otherwise
  (if (not (typep c 'cons)) (return-from try-expand-control-cons nil))
  (if (not (typep (car c) 'symbol)) (return-from try-expand-control-cons nil))
  (if (not (char= #\$ (char (string (car c)) 0))) (return-from try-expand-control-cons nil))
  (cond
    ((string= (string-downcase (car c)) "$loop") (expand-loop-control c))
    ((string= (string-downcase (car c)) "$model") (expand-model-control c))
    (t nil)))

(defun expand-all-control-structures (template)
  "Returns the template with all control structures (i.e. $loop) expanded."
  (cond
    ((listp template)
     (loop for i from 0 for item in template append
          (let ((expanded (try-expand-control-cons item)))
            (if expanded expanded
                ;; If we failed to expand, just treat this as a normal list
                (list (expand-all-control-structures item))))))
    (t template)))

(defun get-root-interpolation-symbol (symbol)
  "Given a symbol like {asd.asd}, returns (and interns if necessary) the keyword
{asd}. Just returns the input if the input wasn't surrounded with {}."
  (let* ((s (string symbol))
         (ix (position #\. s)))
    ;; Check that s is surrounded by {}
    (if (not (and (char= #\} (char s (1- (length s)))) (char= #\{ (char s 0))))
        (return-from get-root-interpolation-symbol symbol))
    ;; Check that s has a '.' in it
    (if (not ix) (return-from get-root-interpolation-symbol (intern s :keyword)))
    ;; Extract the root
    (let* ((root (subseq s 1 ix))
           (surrounded (format nil "{~a}" root)))
      ;; Intern as a keyword
      (intern surrounded :keyword))))

(defun add-chain-from-interpolation-symbol (expanded symbol)
  "Given the expanded result of a symbol and the symbol it was expanded from,
  return an altered expanded symbol which finds the 'child' of the given object.
  For example, if the symbol is {foo.bar.baz}, replace the given expanded symbol
  (which will be something like ($ (<expr to get foo>)) with ($ (@ (<expr to get
  foo>) bar baz)). If the first element of the given expanded isn't $, or if the
  symbol contains no '.', this simply returns the given expanded value with no
  transformation."
  ;; Check that expanded is a cons with $ as the car
  (if (or (not (consp expanded)) (not (eq '$ (car expanded))))
      (return-from add-chain-from-interpolation-symbol expanded))
  (let* ((s (string symbol))
         ;; Unwrap the symbol if it's surrounded with {}
         (unwrapped (if (and (char= #\} (char s (1- (length s)))) (char= #\{ (char s 0)))
                        (subseq s 1 (1- (length s)))))
         ;; Split by '.'
         (splits (str:split "." unwrapped))
         ;; Intern splits
         (splits-symbols (loop for split in splits collect (intern split)))
         ;; Created the altered ps expression
         (altered `($ (@ ,(second expanded) ,@(cdr splits-symbols)))))
    ;; Only return altered if there was more than 1 split (i.e. symbol contained a '.')
    (if (> (length splits) 1) altered expanded)))

(defun expand-with-symbol-table (template symbol-table)
  (cond
    ((typep template 'cons) (loop for child in template collect
                                 (expand-with-symbol-table child symbol-table)))
    ;; Check if this is an event listener symbol (i.e. starting with {@ instead of just {)
    ;; Check if we can expand this symbol
    ((and (typep template 'symbol)
          (and
           (>= (length (string template)) 4)
           (char= #\@ (char (string template) 1))))
     ;; Lookup the non-@ sign symbol in the table, then call that function
     ;; inside a lambda accepting the event
     (let ((expanded (getf symbol-table (get-root-interpolation-symbol (remove #\@ (string template))))))
       (if expanded (add-chain-from-interpolation-symbol `($ (lambda (e) (,expanded vnode e))) template) template)))
    ;; If we're not an event listener symbol, we might just be a normal
    ;; interpolation - check for this
    ((typep template 'symbol)
     (let ((expanded (getf symbol-table (get-root-interpolation-symbol (string template)))))
       (if expanded (add-chain-from-interpolation-symbol expanded template) template)))
    (t template)))

(defun expand-interpolations (template fields)
  "Expand all the interpolations of the given template with the given fields."
  ;; First parse out the fields & generate a symbol table
  (let*
      ((state (loop for (k v) on (getf fields :state) by #'cddr collect (list k v)))
       (computed (getf fields :computed))
       (attrs (getf fields :attrs))
       (methods (getf fields :methods))
       ;; Create a plist of all the state / computed / at... with ps expansions
       (symbol-table
        (append
         '(:{$store} ($ (@ window store)))
         (loop for a in attrs
            append (list
                    (intern (format nil "{~a}" a) :keyword)
                    `($ (@ vnode state ,a))))
         ;; Compile computed property list into symbol table for rendering
         (loop for c in computed
            append (list
                    (intern (format nil "{~a}" (car c)) :keyword)
                    `($ (,(car c) vnode))))
         ;; Compile method list. Don't compile the full method call,
         ;; only the names, allows for more customisation when
         ;; interpolating
         (loop for m in methods
            append (list
                    (intern (format nil "{~a}" (car m)) :keyword)
                    (car m)))
         ;; Compile attribute list into symbol table for rendering
         (loop for s in state
            append (list
                    (intern (format nil "{~a}" (car s)) :keyword)
                    `($ (@ vnode state ,(car s)))
                    )))))
    ;; Expand with our symbol table
    (expand-with-symbol-table template symbol-table)))

(defun defcomp (fields template)
  "
  # Fields
  ## Example
  (defcomp (
    :attrs (first-name last-name)
    :state (count 0)
    :computed (
      (double-count (* 2 {count}))
      (full-name (concatenate 'string {first-name} {last-name}))
    )
    :methods (
      (inc () (setf {count} (1+ {count})))
    ))
    (div
      \" first name: \" {first-name}
      (\"br\")
      \" last name: \" {last-name}
      (\"br\")
      \" count: \" {count}
      (\"br\")
      \" double count: \" {double-count}
      (\"br\")
      \" full name: \" {full-name}
      (\"br\")
      ((button onclick {@inc}) \"Increment\")))
  This defines a component with 2 attributes, a state called 'count', and some
  computed properties. The computed and attrs are used in the template with the { and }
  chars.
  Computed properties are any value parenscript expression. Fields can be
  interpolated into the expression with (), and multiple forms can be evaluated
  inside a progn. Fields can be modified with (setf {field-name} <val>). They
  can be referenced in the template to interpolate the values.
  Methods are made of 3 parts, the name of the method, a list of the parameters,
  and the body (just 1 expression, progn NOT added).
  Methods cannot be interpolated into templates as values, as they might not
  return anything / take parameters. Use computed properties for this. The body
  of the method can contain interpolated values just like the computed properties.

  Using @ to interpolate inc indicates an event listener.

  As far as the generated mithril goes, methods and computed properties are
  stored as functions in the object which take a vnode as a parameter.
  State and attrs are initialised in oninit.

  Attrs are all mutable, unlike in the mithril framework. This is achieved by
  creating extra state fields initialised to the values of the attrs.

  # Template syntax
    Basically just LHTML (see
    https://franz.com/support/documentation/6.0/doc/phtml.htm#lhtml) with some
    extras & no need to use keywords. Symbols surrounded with {} are a
    component field (i.e. attr, state, or funtion call)
    Keywords can be used to reference other component types (when used in the place of a tag)
  ## Examples
    (div
      (p \"This is paragraph 1\")
      (p \"This is paragraph 2\")
      ((p class \"para\") \"This paragraph has the class 'para'\")
      (p \"This paragraph has a link to \" ((a href \"https://google.com\") \"google\") \".\"))
    Fairly explanetory HTML output

    (div (p {my-attr}))
    Will generate
    <div><p>...</p></div> where '...' is the current value of the attribute named 'my-attr'.

    (div ({my-comp} my-attr \"Hello\"))
    Will generate something equivalent to the following(as JSX)
    <div><myComp myAttr=\"Hello\"/></div>
    where myComp is a component which has already been created in this scope
    (with this defcomp macro). 'myAttr' is the name of an attribute on the
    component 'myComp'. Remember this performs kebab to camel case conversions,
    so you would actually define a component named 'my-comp' with an attribute
    named 'my-attr'. "
  ;; Extract data from the fields list
  (let* ((state (loop for (k v) on (getf fields :state) by #'cddr collect (list k v)))
         (computed (getf fields :computed))
         (attrs (getf fields :attrs))
         (methods (getf fields :methods))
         ;; Build state declarations of attrs for parenscript (these will go in the oninit method)
         (attr-state-decl (loop for a in attrs collect `(setf (@ vnode state ,a) (@ vnode attrs ,a))))

         ;; Create computed & method property declarations
         (computed-decl
          (loop for (k v) in computed
             collect `(,k (lambda (vnode) ,(expand-all-ps-injects
                                            (expand-interpolations v fields))))))
         (method-decl
          (loop for (name params body) in methods
             collect `(,name (lambda (vnode ,@params) ,(expand-all-ps-injects
                                                        (expand-interpolations body fields)))))))
    `((lambda ()
        ,@(loop for (k v) in computed-decl collect `(var ,k ,v))
        ,@(loop for (k v) in method-decl collect `(var ,k ,v))
        (create
         ,@(loop for (k v) in computed-decl append (list k k))
         ,@(loop for (k v) in method-decl append (list k k))
         oninit (lambda (vnode)
                  ,@attr-state-decl
                  ,@(loop for (k v) in state collect `(setf (@ vnode state ,k) ,v)))
         view
         (lambda (vnode)
           ,(let*
             ( ;; Expand out all control cons
              (cc-expanded (expand-all-control-structures template))
              ;; Replace {} symbols with actual vnode accesses
              ({}-expanded (expand-interpolations cc-expanded fields)))
             ;; Render with the symbol table (expand control structures first)
             (render {}-expanded))))))))

(defun expand-all-ps-injects (e)
  "Used by try-expand-ps-inject to expand all $ inside a given block. This is
for nested $."
  (if (listp e)
      (if (listp (car e))
          (loop for child in e collect (expand-all-ps-injects child))
          (if (string= (string (car e)) "$") (expand-all-ps-injects (second e))
              (loop for child in e collect (expand-all-ps-injects child))))
      e))

(defun try-expand-ps-inject (e &optional default)
  "Given an expression, checks whether it's a list with car $, if so returns the
cdr, otherwise returns nil."
  (if (typep e 'list)
      (if (typep (car e) 'list) default
          (if (string= (string (car e)) "$") (expand-all-ps-injects (second e)) default)) default))


(defun render (lhtml)
  "Renders LHTML style lisp HTML lists into a parenscript mithril object to
    return from a component's view function. The difference is you don't need
    keywords.

    Example:
    (div
      (p \"This is paragraph 1\")
      (p \"This is paragraph 2\")
      ((p class \"para\") \"This paragraph has the class 'para'\")
      (p \"This paragraph has a link to \" ((a href \"https://google.com\") \"google\") \".\"))
    Generates the mithril code to generate:
    <div>
      <p>This is paragraph 1</p>
      <p class=\"para\">This is paragraph 2</p>
      <p>This paragraph has the class 'para'</p>
      <p>This paragraph has a link to <a href=\"https://google.com\">.</a></p>
    </div>

    Lookups into the symbol table can be accessed by surrounding the value with
    {}. For example:
    (div (p {my-attr}))
    will lookup the :my-attr value in the given symbol-table plist and insert
    that parenscript expression into the result.
    "
  ;; First, check if we can expand this into a parenscript expression
  (let ((expanded (try-expand-ps-inject lhtml)))
    (if expanded expanded
        (cond
          ;; Check if this is a list
          ((typep lhtml 'list)
           (let* ((raw-tag (if (typep (car lhtml) 'list) (caar lhtml) (car lhtml)))
                  (tag (if (typep raw-tag 'keyword)
                           (intern (string raw-tag))
                           (string raw-tag)))
                  (attr-list (loop for (k v) on (if (typep (car lhtml) 'list) (cdar lhtml) nil) by #'cddr
                                append
                                  (list k (try-expand-ps-inject v v))))
                  (children (cdr lhtml)))
             ;; Assemble into final parenscript mithril markup
             `(m ,tag
                 ,(append '(create) attr-list)
                 ,(append '(array) (loop for c in children collect (render c))))))
          ;; If just a symbol intern if a keyword (because it'll be a component)
          ;; or just passthrough otherwise
          ((typep lhtml 'symbol)
           (if (typep lhtml 'keyword) `(m ,(intern (string lhtml))) lhtml))
          (t lhtml)))
    ))


(defparameter *default-error-component* (defcomp () '(div {$store.rdf-app-error})))
(defparameter *comp-list* ())
(defparameter *error-component* *default-error-component*)
(defparameter *routes* ())
(defparameter *init-state* ())
(defparameter *lass-styles* ())

(defun add-initial-store-state (key value)
  "Add a key to the initial store state, and store the given PS value in it.
  Store values can be accessed with the {$store} interpolation.
  # Example
  (add-initial-store-state 'count 0)"
  (setf (getf *init-state* key) value))


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
            (setf (@ window store) (create ,@*init-state*))
            ;; Mount mithril routes
            (let* (append (root (chain document (get-element-by-id "rdf-content")))
                          (error-flash (chain document (get-element-by-id "rdf-error-flash")))
                          (error-component ,*error-component*)
                          ,@(reverse (loop for (k v) on *comp-list* by #'cddr collect (list k v))))
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
  (lass:compile-and-write (loop for (k v) on *lass-styles* by #'cddr append v)))

(defun render-app-js ()
  "Given the current state (*routes* and *comp-list*), render an appropriate
app.js file & return it as a string"
  (app *routes*))
