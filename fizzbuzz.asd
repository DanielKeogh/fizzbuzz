;;;; fizzbuzz.asd

(asdf:defsystem #:fizzbuzz
  :description "A fizzbuzz implementation"
  :author "keogh.daniel@gmail.com"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria :trivial-gray-streams :cl-speedy-queue)
  :components ((:file "package")
	       (:file "stream-wrapper")
	       (:file "fizzler-lang")
               (:file "fizzbuzz")))
