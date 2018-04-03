(in-package :rdf-full-example)

(defun register-components ()
  (rdf:register-component
   :nav ()
   '(div
     ((a href "/#!") "Home")
     ((a href "/#!/about") "About")
     ))

  (rdf:add-initial-store-state 'home '(create count 0))
  (rdf:add-initial-store-state 'about '(create count 0))

  (rdf:register-component
   :home
   '(
     :state (big-object (create a 1 b 2 c 3))
     :methods ((inc () (setf {$store.home.count} (1+ {$store.home.count})))))
   '(div :nav (div "Hello, this is the home page.")
     ((button onclick {@inc}) "You've pressed this button " {$store.home.count} " times.")
     (br)
     "A: " {big-object.a} (br)
     "B: " {big-object.b} (br)
     "C: " {big-object.c} (br)
     ))

  (rdf:register-component
   :about
   '(
     :methods ((inc () (setf {$store.about.count} (1+ {$store.about.count})))))
   '(div :nav (div "Hello, this is the about page. It has a much bigger button.")
     ((button style "font-size: 30px" onclick {@inc}) "You've pressed this button " {$store.about.count} " times."))))

(defun setup-routes ()
  (rdf:set-view-routes '(("/" home)
                        ("/about" about))))

(defun main () 
  (register-components)
  (setup-routes)
  (rdf:rdf-stop)
  (rdf:rdf-start))
