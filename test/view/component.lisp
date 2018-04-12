(in-package :rdf)

(prove:plan 3)
(prove:is (get-interpolation-chain '{!store.foo.bar}) "FOO.BAR")
(prove:is (get-interpolation-chain '{!store.foo}) "FOO")
(prove:is (get-interpolation-chain '{!store}) nil)
(prove:finalize)

(prove:plan 1)
(prove:is-error (error-check-fields '(:attr ((my-attr 0)))) 'error
                "error-check-fields should recognise mispelt fields")
(prove:is-error (error-check-fields '(:state ((my-attr)))) 'error
                "error-check-fields should recognise state without a default value")
(prove:is-error (error-check-fields '(:state (my-attr 0))) 'error
                "error-check-fields should recognise fields which aren't alists")
(prove:is-error (error-check-fields '(:attrs ((my-attr 0 1)))) 'error
                "error-check-fields should recognise attrs with bad number of default vals")
(prove:is-error (error-check-fields '(:methods ((my-method (body))))) 'error
                "error-check-fields should recognise methods with bad number of default vals")
(prove:is-error (error-check-fields '(:computed ((my-computed () (body))))) 'error
                "error-check-fields should recognise computeds with bad number of default vals")
(prove:is-error (error-check-fields '(:lifecycle ((oninit () (body))))) 'error
                "error-check-fields should recognise lifecycle methods with bad
                number of default vals")
(prove:is-error (error-check-fields '(:lifecycle ((onint () (body))))) 'error
                "error-check-fields should recognise misspelt lifecycle methods")
(prove:is (error-check-fields '(:state ((my-attr 0)))) nil
          "error-check-fields should recognise correct field specs")
(prove:finalize)
