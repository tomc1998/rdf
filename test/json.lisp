(in-package :rdf)

;; Test JSON

(prove:plan 4)
(prove:is (is-plist (list :a 1 :b 2 :c "Hello")) T)
(prove:is (is-plist (list 1 2 "Hello")) NIL)
(prove:is (is-plist (list :a 1 :b 2  "Hello")) NIL)
(prove:is (is-plist (list :a 1 :b 2 :c)) NIL)
(prove:finalize)

(prove:plan 9)
(prove:is (to-json "Hello") "\"Hello\"")
(prove:is (to-json 3) "3")
(prove:is (to-json 0) "0")
(prove:is (to-json '(0 1 2 3)) "[0,1,2,3]")
(prove:is (to-json '(0 1 2 "Hello")) "[0,1,2,\"Hello\"]")
(prove:is (to-json '(:a 1 :b 2 :c 3)) "{a:1,b:2,c:3}")
(prove:is (to-json '(:a (1 2 3) :b "Hello")) "{a:[1,2,3],b:\"Hello\"}")
(prove:is (to-json '(1 2 3 (:a (1 2 3) :b "Hello"))) "[1,2,3,{a:[1,2,3],b:\"Hello\"}]")
(prove:is (to-json '(:a 1 :b 2 :c 3 :d (:a (1 2 3) :b "Hello"))) "{a:1,b:2,c:3,d:{a:[1,2,3],b:\"Hello\"}}")
(prove:is-error (to-json #'car) 'malformed-to-json-input)
(prove:finalize)
