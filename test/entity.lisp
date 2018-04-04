(in-package :rdf)

(prove:plan 2)
(defentity user ((first-name "VARCHAR(256)") (last-name "VARCHAR(256)")) ())
(let ((user (entity-from-json 'user '(:first-name "Tom" :last-name "Cheng"))))
  (prove:is (slot-value user 'first-name) "Tom" "entity-from-json should work")
  (prove:is (slot-value user 'last-name) "Cheng" "entity-from-json should work"))
(prove:finalize)
