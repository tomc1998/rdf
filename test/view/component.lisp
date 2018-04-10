(in-package :rdf)

(prove:plan 1)
(prove:is (get-interpolation-chain '{!store.foo.bar}) "foo.bar")
(prove:is (get-interpolation-chain '{!store.foo}) "foo")
(prove:is (get-interpolation-chain '{!store}) nil)
(prove:finalize)
