(asdf:defsystem #:advent-of-code
  :description "Advent of Code 2024 solutions in Common Lisp"
  :version "0.1"
  :author "Jay Burkhardt - jeremy.burkhardt@gmail.com"
  :license "MIT"
  :depends-on (#:cl-ppcre #:uiop)
  :serial t
  :components
  ((:file "packages")
   (:module "day1"
     :components ((:file "solution")))
   (:module "day2"
     :components ((:file "solution")))
   (:module "day3"
     :components ((:file "solution")))))
