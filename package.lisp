;;;; package.lisp

(defpackage #:yuri
  (:use #:cl #:alexandria #:serapeum #:optima #:optima.ppcre)
  (:export #:uri #:parse-uri
           #:null-uri
           #:valid-uri
           #:invalid-uri
           #:fake-uri #:make-fake-uri
           #:fake-response #:fake-response-values
           #:data-uri
           #:match-uri
           #:uri->string #:uri= #:invalid-uri?
           #:parse-valid-uri #:parse-feed-uri
           #:merge-uris #:enough-uri
           #:uri-relative? #:uri-absolute?
           #:uri-host-uri #:uri-domain
           #:uri->https #:ensure-https
           #:canonicalize-uri
           #:uri-scheme #:uri-path #:uri-domain
           #:strip-hash
           #:uri<=
           #:uri-path
           #:uri-directory
           #:uri-fragment
           #:extract-tld

           #:strip-utm
           #:extract-domain))


