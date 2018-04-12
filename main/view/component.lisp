(in-package :rdf)

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
  (which will be something like (! (<expr to get foo>)) with (! (@ (<expr to get
  foo>) bar baz)). If the first element of the given expanded isn't !, or if the
  symbol contains no '.', this simply returns the given expanded value with no
  transformation."
  ;; Check that expanded is a cons with ! as the car
  (if (or (not (consp expanded)) (not (eq '! (car expanded))))
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
         (altered `(! (@ ,(second expanded) ,@(cdr splits-symbols)))))
    ;; Only return altered if there was more than 1 split (i.e. symbol contained a '.')
    (if (> (length splits) 1) altered expanded)))

(defun get-interpolation-chain (template)
  ;; Split by . then concatenate
  (reduce (lambda (s0 s1) (concatenate 'string s0 "." s1))
          (let ((rest (cdr
                       (str:split
                        "." (subseq (string template)
                                    1 (1- (length (string template))))))))
            (if rest rest (return-from get-interpolation-chain nil)))))

(defun expand-with-symbol-table (template symbol-table)
  (let ((final-expansion) (root-interpolation))
    (cond
      ((typep template 'cons) (setf final-expansion
                                    (loop for child in template collect
                                         (expand-with-symbol-table child symbol-table))))
      ;; Check if this is an event listener symbol (i.e. starting with {@ instead of just {)
      ;; Check if we can expand this symbol
      ((and (typep template 'symbol)
            (and
             (>= (length (string template)) 4)
             (char= #\@ (char (string template) 1))))
       ;; Lookup the non-@ sign symbol in the table, then call that function
       ;; inside a lambda accepting the event
       (setf root-interpolation (get-root-interpolation-symbol (remove #\@ (string template))))
       (let ((expanded (getf symbol-table root-interpolation)))
         (if expanded (setf final-expansion
                                              (add-chain-from-interpolation-symbol
                                               `(! (lambda (e) (,expanded vnode e))) template))
                   (setf final-expansion template))))
      ;; If we're not an event listener symbol, we might just be a normal
      ;; interpolation - check for this
      ((typep template 'symbol)
       (setf root-interpolation (get-root-interpolation-symbol (string template)))
       (let ((expanded (getf symbol-table root-interpolation)))
         (if expanded (setf final-expansion (add-chain-from-interpolation-symbol expanded template))
             (setf final-expansion template))))
      (t (setf final-expansion template)))
    ;; If root interpolation is !store, check if it's a computed property - if so,
    ;; generate a function call instead of just a data access
    (if (and root-interpolation (eq root-interpolation :{!store}))
        (let ((template-chain (get-interpolation-chain template)))
          (if (not template-chain) (return-from expand-with-symbol-table final-expansion))
          (let ((store-computed (getf *store-computed* (intern template-chain :keyword))))
            (if store-computed
                ;; If we found a computed value, return the function call result
                ;; of final-expansion
                `(! (,final-expansion))
                final-expansion)))
        final-expansion)))

(defun expand-interpolations (template fields)
  "Expand all the interpolations of the given template with the given fields."
  ;; First parse out the fields & generate a symbol table
  (let*
      ((state (loop for (k v) in (getf fields :state) collect (list k v)))
       (computed (getf fields :computed))
       (attrs (getf fields :attrs))
       (methods (getf fields :methods))
       ;; Create a plist of all the state / computed / at... with ps expansions
       (symbol-table
        (append
         '(:{children} (! (@ vnode children)))
         '(:{!store} (! (@ window store)))
         (loop for (a nil) in attrs
            append (list
                    (intern (format nil "{~a}" a) :keyword)
                    `(! (@ vnode state ,a))))
         ;; Compile computed property list into symbol table for rendering
         (loop for c in computed
            append (list
                    (intern (format nil "{~a}" (car c)) :keyword)
                    `(! (,(car c) vnode))))
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
                    `(! (@ vnode state ,(car s)))
                    )))))
    ;; Expand with our symbol table
    (expand-with-symbol-table template symbol-table)))

(defun defcomp (fields template)
  "
  This should in general not be used by app devs - use the register-component
  wrapper instead. This will make sure your component is added to the served .js
  file. This function simply generates a parenscript expression to create a new
  component type, and doesn't make sure this parenscript expression is rendered
  into the served data.

  # Fields
  ## Example
  (defcomp (
    :lifecycle ((oninit (chain console (log \"Hello, this is the component init\"))))
    :attrs ((first-name nil) (last-name nil))
    :state ((count 0))
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
  This defines a component with 2 attributes (defaulting to nil),
  a state called 'count', and some computed properties. The computed and attrs are
  used in the template with the { and } chars. 'Dotted' notation can be used to
  access children in an object value. For example: {foo.bar} will access the
  'foo' field, and access the 'bar' child of this.
  Computed properties are any value parenscript expression. Fields can be
  interpolated into the expression with (), and multiple forms can be evaluated
  inside a progn. Fields can be modified with (setf {field-name} <val>). They
  can be referenced in the template to interpolate the values.
  Methods are made of 3 parts, the name of the method, a list of the parameters,
  and the body (just 1 expression, progn NOT added).
  Methods cannot be interpolated into templates as values, as they might not
  return anything / take parameters. Use computed properties for this. The body
  of the method can contain interpolated values just like the computed properties.

  Lifecycle methods can be defined inside :lifecycle in an assoc list. See the
  examples & mithril documentation for all lifecycle method documentation. vnode
  is automatically in scope, and interpolations can be used.
  Available lifecycle methods:
  :oninit
  :onupdate
  :onbeforeupdate
  :onremove
  :onbeforeremove
  :oncreate

  Using @ to interpolate inc indicates an event listener.

  As far as the generated mithril goes, methods and computed properties are
  stored as functions in the object which take a vnode as a parameter.
  State and attrs are initialised in oninit.

  Attrs are all mutable, unlike in the mithril framework. This is achieved by
  creating extra state fields initialised to the values of the attrs.

  # Special interpolations
  {children} can be used for inserting this component's children. This allow
  syou to create 'container' components, which can have children inserted like
  any normal element.

  {!store} can be used to access the store. Dotted notation can be used here,
  for example: {!store.foo} would access the 'foo' field of the store object.

  # Extra templating controls
  A cons can be inserted with the car being a symbol beginning with !. This will
  be expanded when compiling the template. Here are some examples:

  (!loop for u in users (div \"Hello\", {u.first-name}))
  This will be expanded to a list of divs containing 'Hello, XXX' where XXX is
  the name of the user. 'users' should be a field of the current component,
  containing an array of objects, which all have the 'first-name' field.

  (!model {my-value})
  This can be placed as an attribute, and will bind the data of the element
  (let's say, and <input>) to the model & update the model when the element
  changes. This expands to bind the 'value' and 'onchange' attributes on the
  element. As such, this shouldn't be used if value or onchange is already used.
  Here is a full template example:
  ((input (!model {my-value}) placeholder \"Input a value here\"))

  (!if {my-condition} (div \"my-condition is true\") (span \"my-condition is false\"))
  You can perform conditional rendering with this. If the given condition is
  truthy, the first item (in this case a div) is inserted into the rendered
  markup. If the condition is falsey, the second item (in this case a span) is
  inserted into the markup. This can also be used in attributes:
  ((div class (!if {my-condition} \"some-class\")) \"Hello)
  If 'my-condition' is true, this will apply 'some-class' to the element.

  (!class \"class0 class1\" (!if {my-condition} \"class2\" \"class3\"))
  This allows for more complex class application. !class takes a varargs list of
  strings and !if controls. It applies the !if classes conditionally based on
  the given condition, and appends all the strings together separating by a
  space.
  Strings can also be interpolated, for example:
  (!class {my-class-attr})
  Where my-class-attr is some string field.

  # Template syntax
    Basically just LHTML (see
    https://franz.com/support/documentation/6.0/doc/phtml.htm#lhtml) with some
    extras & no need to use keywords. Symbols surrounded with {} are a
    component field (i.e. attr, state, or funtion call)
    Other components can be referenced by name. See examples.
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
    named 'my-attr'."
  ;; Extract data from the fields list
  (let* ((state (getf fields :state))
         (computed (getf fields :computed))
         (attrs (getf fields :attrs))
         (methods (getf fields :methods))
         (lifecycle (getf fields :lifecycle))
         ;; get lifecycle method bodies
         (onupdate (mapcar (lambda (a) (expand-all-ps-injects (expand-interpolations a fields)))
                           (cdr (assoc 'onupdate lifecycle :test
                                       (lambda (s0 s1) (string= (string s0) (string s1)))))))
         (onbeforeupdate (mapcar (lambda (a) (expand-all-ps-injects (expand-interpolations a fields)))
                                 (cdr (assoc 'onbeforeupdate lifecycle :test
                                             (lambda (s0 s1) (string= (string s0) (string s1)))))))
         (onremove (mapcar (lambda (a) (expand-all-ps-injects (expand-interpolations a fields)))
                           (cdr (assoc 'onremove lifecycle :test
                                       (lambda (s0 s1) (string= (string s0) (string s1)))))))
         (onbeforeremove (mapcar (lambda (a) (expand-all-ps-injects (expand-interpolations a fields)))
                                 (cdr (assoc 'onbeforeremove lifecycle :test
                                             (lambda (s0 s1) (string= (string s0) (string s1)))))))
         (oncreate (mapcar (lambda (a) (expand-all-ps-injects (expand-interpolations a fields)))
                           (cdr (assoc 'oncreate lifecycle :test
                                       (lambda (s0 s1) (string= (string s0) (string s1)))))))
         (oninit (mapcar (lambda (a) (expand-all-ps-injects (expand-interpolations a fields)))
                         (cdr (assoc 'oninit lifecycle :test
                                     (lambda (s0 s1) (string= (string s0) (string s1)))))))
         ;; Build state declarations of attrs for parenscript (these will go in the oninit method)
         (attr-state-decl (loop for (a default) in attrs collect
                               `(setf (@ vnode state ,a)
                                      ;; If attr defined, use that - otherwise, use the default
                                      (if (@ vnode attrs ,a) (@ vnode attrs ,a) ,default))))

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

         ,@(if onupdate `(onupdate (lambda (vnode) ,@onupdate)))
         ,@(if onbeforeupdate `(onbeforeupdate (lambda (vnode) ,@onbeforeupdate)))
         ,@(if onremove `(onremove (lambda (vnode) ,@onremove)))
         ,@(if onbeforeremove `(onbeforeremove (lambda (vnode) ,@onbeforeremove)))
         ,@(if oncreate `(oncreate (lambda (vnode) ,@oncreate)))

         ;; Lifecycle methods
         oninit (lambda (vnode)
                  ,@attr-state-decl
                  ,@(loop for (k v) in state collect `(setf (@ vnode state ,k) ,v))
                  ,@oninit)
         view
         (lambda (vnode)
           ,(let*
             (;; Expand out all control cons
              (cc-expanded (expand-all-control-structures template))
              ;; Replace {} symbols with actual vnode accesses
              ({}-expanded (expand-interpolations cc-expanded fields)))
             ;; Render with the symbol table (expand control structures first)
             (render {}-expanded))))))))
