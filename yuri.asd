;;;; yuri.asd

(asdf:defsystem #:yuri
  :description "Defused URIs."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum
               #:optima
               #:optima.ppcre
               #:cl-algebraic-data-type
               #:puri
               #:quri
               #:fset
               #:cl-tld)
  :version "2.0.1"
  :in-order-to ((asdf:test-op (asdf:test-op #:yuri/test)))
  :components ((:file "package")
               (:file "yuri" :depends-on ("package"))))

(asdf:defsystem #:yuri/test
  :description "Test system for Yuri."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:yuri #:fiveam)
  :pathname "test"
  :perform (asdf:test-op (o c) (uiop:symbol-call :yuri.test :run-tests))
  :components ((:file "test")))

