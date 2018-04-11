(in-package :rdf)

(prove:plan 1)
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
