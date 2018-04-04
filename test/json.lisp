(in-package :rdf)

(prove:plan 2)
(prove:is (kebab-to-camel-case "my-name-is") "myNameIs")
(prove:is (kebab-to-camel-case "mynameis") "mynameis")
(prove:finalize)

(prove:plan 4)
(prove:is (is-plist (list :a 1 :b 2 :c "Hello")) T)
(prove:is (is-plist (list 1 2 "Hello")) NIL)
(prove:is (is-plist (list :a 1 :b 2  "Hello")) NIL)
(prove:is (is-plist (list :a 1 :b 2 :c)) NIL)
(prove:finalize)

;; Test serialising JSON
(prove:plan 10)
(prove:is (to-json "Hello") "\"Hello\"")
(prove:is (to-json 3) "3")
(prove:is (to-json 0) "0")
(prove:is (to-json '(0 1 2 3)) "[0,1,2,3]")
(prove:is (to-json '(0 1 2 "Hello")) "[0,1,2,\"Hello\"]")
(prove:is (to-json '(:a 1 :b 2 :c 3)) "{\"a\":1,\"b\":2,\"c\":3}")
(prove:is (to-json '(:a (1 2 3) :b "Hello")) "{\"a\":[1,2,3],\"b\":\"Hello\"}")
(prove:is (to-json '(1 2 3 (:a (1 2 3) :b "Hello")))
          "[1,2,3,{\"a\":[1,2,3],\"b\":\"Hello\"}]")
(prove:is (to-json '(:a 1 :b 2 :c 3 :d (:a (1 2 3) :b "Hello")))
          "{\"a\":1,\"b\":2,\"c\":3,\"d\":{\"a\":[1,2,3],\"b\":\"Hello\"}}")
(prove:is-error (to-json #'car) 'malformed-to-json-input)
(prove:finalize)

;; Test deserialising JSON
(defmacro from-json-test-helper (j)
  `(let ((j ',j)) (prove:is (from-json (to-json j)) j (format nil "from-json test: ~a" j)))
  )

(prove:plan 8)
(from-json-test-helper (1 2 3))
(from-json-test-helper ("Hello" 1 2 3 (1 2 3 "Hello")))
(from-json-test-helper (:a 1 :b 2 :c ("Hello" "My" "Name" "Is" "Tom")))
(from-json-test-helper (:a (:a (:a 1))))
(from-json-test-helper 1)
(from-json-test-helper T)
(from-json-test-helper NIL)
(from-json-test-helper "Hello")
(prove:finalize)
