(in-package :rdf)

(prove:plan 2)
(prove:is (expand-with-symbol-table '(div {foo}) '(:{foo} ($ (@ vnode state foo))))
          '(div ($ (@ vnode state foo))))
(prove:is (expand-with-symbol-table '(div {foo.bar}) '(:{foo} ($ (@ vnode state foo))))
          '(div ($ (@ (@ vnode state foo) bar))))
(prove:finalize)

(prove:plan 6)
(prove:is (get-root-interpolation-symbol '{asd.fgh}) :{asd} "Getting the root of an interpolation should work")
(prove:is (get-root-interpolation-symbol '{asd}) :{asd} "Getting the root of an interpolation should work")
(prove:is (get-root-interpolation-symbol 'asd) 'asd "Getting the root of an interpolation should work")
(prove:is (get-root-interpolation-symbol :asd) :asd "Getting the root of an interpolation should work")
(prove:is (get-root-interpolation-symbol '{asd.123.234.345}) :{asd} "Getting the root of an interpolation should work")
(prove:is (get-root-interpolation-symbol '{@asd.123.234.345}) :{@asd} "Getting the root of an interpolation should work")
(prove:finalize)

(prove:plan 3)
(prove:is (add-chain-from-interpolation-symbol '($ (vnode attrs asd)) '{asd.foo.bar})
          '($ (@ (vnode attrs asd) foo bar))
          "add-chain-from-interpolation-symbol should add the appropriate
          accesses to the second attribute of the given list")
(prove:is (add-chain-from-interpolation-symbol '($ (hello (my (name is tom)))) '{asd.foo.bar})
          '($ (@ (hello (my (name is tom))) foo bar))
          "add-chain-from-interpolation-symbol should work with any arbitrary expanded second value")
(prove:is (add-chain-from-interpolation-symbol '(vnode attrs asd) '{asd.foo.bar})
          '(vnode attrs asd)
          "add-chain-from-interpolation-symbol should passthrough when expanded car is not $")
(prove:finalize)
