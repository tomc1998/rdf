(in-package :rdf)

;; !loop control cons tests
(prove:plan 2)
(prove:is (expand-loop-control '(!loop for x in {asd} (div {asd})))
          '((!
             (let ((elements (array)))
               (loop for item in {asd} do
                    (progn
                      (chain elements
                             (push (m "DIV" (create) (array {asd}))))))
               elements)))
          "!loop cc should work")
(prove:is (expand-loop-control '(!loop for x in {asd} (div {asd}) (div "Hello")))
          '((!
             (let ((elements (array)))
               (loop for item in {asd} do
                    (progn (chain elements (push (m "DIV" (create) (array {asd}))))
                           (chain elements (push (m "DIV" (create) (array "Hello"))))))
               elements)
             ))
          "!loop cc should work with multiple body elements")
(prove:finalize)

(prove:plan 1)
(prove:is (expand-model-control '(!model {asd}))
          '(onchange (! (chain m (with-attr "value" (lambda (v) (setf {asd} v)))))
            value {asd})
          "expand-model-control works")
(prove:finalize)

(prove:plan 4)
(prove:is (expand-if-control '(!if {asd} ((div style (create width "100%")) "Hello") (div "Hello")))
          '((! (if {asd}
                   (m "DIV" (create style (create width "100%")) (array "Hello"))
                   (m "DIV" (create) (array "Hello")))))
          "expand-if-control works with if & else statement")
(prove:is (expand-if-control '(!if {asd} ((div style (create width "100%")) "Hello")))
          '((! (if {asd}
                   (m "DIV" (create style (create width "100%")) (array "Hello")))))
          "expand-if-control works with just if statement")
(prove:is (expand-if-control '(!if (= (+ 2 3) 5) ((div style (create width "100%")) "Hello")))
          '((! (if (= (+ 2 3) 5)
                   (m "DIV" (create style (create width "100%")) (array "Hello")))))
          "expand-if-control works with arbitrary parenscript as the condition")
(prove:is (expand-if-control
           '(!if {asd} ((div (!model {asd})) "Hello")))
          '((! (if {asd}
                   (m "DIV" (create onchange
                                    (chain m (with-attr "value" (lambda (v) (setf {asd} v))))
                                    value {asd})
                      (array "Hello")))))
          "expand-if-control works with nested control cons")
(prove:finalize)

(prove:plan 3)
(prove:is (expand-class-control '(!class "my class here"))
          '((! (reduce (lambda (s0 s1) (concatenate 'string s0 " " s1))
                (array "my class here"))))
          "class control expands string into just a string")
(prove:is (expand-class-control '(!class "my" "class" "here"))
          '((! (reduce (lambda (s0 s1) (concatenate 'string s0 " " s1))
                (array "my" "class" "here"))))
          "class control concatenates multiple strings")
(prove:is (expand-class-control '(!class "my" "class" (!if {asd} "here" "HERE")))
          '((! (reduce (lambda (s0 s1) (concatenate 'string s0 " " s1))
                (array "my" "class" (! (if {asd} "here" "HERE"))))))
          "class control works with !if statements")
(prove:finalize)

(prove:plan 4)
(prove:is (try-expand-control-cons '(!if {asd} "asd"))
          (expand-if-control '(!if {asd} "asd"))
          "try-expand-control-cons works for !if")
(prove:is (try-expand-control-cons '(!loop for x in {my-list} (div x)))
          (expand-loop-control '(!loop for x in {my-list} (div x)))
          "try-expand-control-cons works for !loop")
(prove:is (try-expand-control-cons '(!model {asd}))
          (expand-model-control '(!model {asd}))
          "try-expand-control-cons works for !model")
(prove:is (try-expand-control-cons '(!class "my class" (!if {asd} "other classes")))
          (expand-class-control '(!class "my class" (!if {asd} "other classes")))
          "try-expand-control-cons works for !class")
(prove:finalize)

;; is-control-cons tests
(prove:plan 10)
(prove:is (is-control-cons '(!if {asd} "Hello")) t
          "is-control-cons correctly identifies a cc")
(prove:is (is-control-cons '(!loop for x in x)) t
          "is-control-cons correctly identifies a cc")
(prove:is (is-control-cons '(!model {asd})) t
          "is-control-cons correctly identifies a cc")
(prove:is (is-control-cons '(!class "a class" (!if {asd} x))) t
          "is-control-cons correctly identifies a cc")
(prove:is (is-control-cons '(!something asd)) nil
          "is-control-cons doesn't identify invalid cc word")
(prove:is (is-control-cons '(!loop-asd asd)) nil
          "is-control-cons doesn't identify invalid cc word")
(prove:is (is-control-cons '(asdasd asd)) nil
          "is-control-cons doesn't identify invalid cc word")
(prove:is (is-control-cons '((asd asd) asd)) nil
          "is-control-cons returns nil when given a cons with a car as a cons")
(prove:is (is-control-cons ()) nil
          "is-control-cons returns nil when given nil")
(prove:is (is-control-cons 1) nil
          "is-control-cons returns nil when given something that's not a list")
(prove:finalize)

;; expand-all-control-structures tests
(prove:plan 3)
(prove:is (expand-all-control-structures '(!if {asd} "Hello 1" "Hello 2"))
          '(! (if {asd} "Hello 1" "Hello 2"))
          "expand-all-control-structures should work")
(prove:is (expand-all-control-structures '(!if {asd} (!if {asd} "Hello 0" "Hello 1") "Hello 2"))
          '(! (if {asd} (if {asd} "Hello 0" "Hello 1") "Hello 2"))
          "expand-all-control-structures should work with nested structures")
(prove:is (expand-all-control-structures '(!if {asd} (div "Hello")))
          '(! (if {asd} (m "DIV" (create) (array "Hello"))))
          "expand-all-control-structures should work with full rendered elements")
(prove:finalize)
