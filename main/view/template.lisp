(in-package :rdf)

(defun expand-all-ps-injects (e)
  "Used by try-expand-ps-inject to expand all ! inside a given block. This is
for nested !."
  (if (listp e)
      (if (listp (car e))
          (loop for child in e collect (expand-all-ps-injects child))
          (if (string= (string (car e)) "!") (expand-all-ps-injects (second e))
              (loop for child in e collect (expand-all-ps-injects child))))
      e))

(defun try-expand-ps-inject (e &optional default)
  "Given an expression, checks whether it's a list with car !, if so returns the
cdr, otherwise returns nil."
  (if (typep e 'list)
      (if (typep (car e) 'list) default
          (if (string= (string (car e)) "!") (expand-all-ps-injects (second e)) default)) default))

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
