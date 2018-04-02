(in-package :rdf-full-example)

(defun register-components ()
  (rdf:register-component
   :nav ()
   '(div
     ((a href "/#!") "Home")
     ((a href "/#!/about") "About")
     ))

  (rdf:register-component
   :home
   '(
     :state (count 0
             big-object (create a 1 b 2 c 3))
     :methods ((inc () (setf {count} (1+ {count})))))
   '(div :nav (div "Hello, this is the home page.")
     ((button onclick {@inc}) "You've pressed this button " {count} " times.")
     (br)
     "A: " {big-object.a} (br)
     "B: " {big-object.b} (br)
     "C: " {big-object.c} (br)
     ))

  (rdf:register-component
   :about
   '(
     :state (count 0)
     :methods ((inc () (setf {count} (1+ {count})))))
   '(div :nav (div "Hello, this is the about page. It has a much bigger button.")
     ((button style "font-size: 30px" onclick {@inc}) "You've pressed this button " {count} " times."))))

(defun setup-routes ()
  (rdf:set-view-routes '(("/" home)
                        ("/about" about))))

(defun main () 
  (register-components)
  (setup-routes)
  (rdf:rdf-stop)
  (rdf:rdf-start))
