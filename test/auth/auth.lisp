(prove:plan 1)

(rdf:setup-auth '((first-name "VARCHAR(256)") (last-name "VARCHAR(256)"))
                :auth-types '(:email-password) :override t)

(prove:is (print (mapcar #'sb-mop:slot-definition-name
                         (sb-mop:class-direct-slots
                          (class-of (make-instance 'rdf:user-auth)))))
          '(rdf:id rdf::email rdf::pass rdf::parent-user-info-id)
          "setup-auth correctly creates user-auth entity"
          )

(prove:is 'rdf::parent-user-info-id 'corm::parent-user-info-id)

(prove:is (print (mapcar #'sb-mop:slot-definition-name
                   (sb-mop:class-direct-slots
                    (class-of (make-instance 'rdf:user-info)))))
          '(rdf:id first-name last-name)
          "setup-auth correctly creates user-info entity")

(prove:finalize)
