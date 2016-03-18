;;;; yuri.lisp

(in-package #:yuri)

;;; "yuri" goes here. Hacks and glory await!

;; “All conditions serious enough to require interactive intervention
;; if not handled should inherit from the type ‘serious-condition’.”
(deftype invalid-uri-error ()
  'serious-condition)

(def null-uri-string "#")

(deftype uri-part () '(or null string))

(deftype path-head () '(member :relative :absolute))

(defconst http "http")

(defconst https "https")

(defgeneric parse-uri (source)
  (:documentation "Parse SOURCE into a URI."))

(defun uri (source)
  "Parse SOURCE into a URI."
  (parse-uri source))

(defclass fake-response ()
  ((body :initarg :body)
   (status-code :initarg :status-code :initarg :status :type (integer 0 599))
   (headers :initarg :headers :type list)
   (final-location :initarg :final-location :type (or string quri:uri)
                   :accessor final-location)
   (error :initarg :error))
  (:documentation "A fake URI for testing. It contains all of the
  information that would be returned from a request.")
  (:default-initargs
   :body nil
   :status-code 200
   :headers '()
   :error nil
   :final-location (quri:uri "http://example.com")))

(defmethod fake-response-values ((self fake-response))
  (with-slots (body status-code headers final-location error) self
    (if error (funcall error)
        (values body status-code headers final-location))))

(adt:defdata uri
  null-uri
  (valid-uri quri:uri)
  (invalid-uri string invalid-uri-error)
  (fake-uri fake-response)
  (data-uri string))

(defmacro match-uri (x &body body)
  ;; Ideally we would stack-allocate the URI if we were constructing a
  ;; new one.
  `(adt:match uri (parse-uri ,x)
     ;; Handle (or ...) clauses.
     ,@(loop for (match . body) in body
             if (and (listp match)
                     (eql (car match) 'or))
               append (loop for term in (cdr match)
                            collect (cons term body))
             else collect (cons match body))))

(defun make-fake-uri (&rest args)
  (fake-uri (apply #'make 'fake-response args)))

(defmethod print-object :around ((self uri) stream)
  (if *print-escape* (call-next-method)
      (adt:match uri self
        (null-uri
         (write-string null-uri-string stream))
        (fake-uri
         (call-next-method))
        ((valid-uri quri)
         (quri:render-uri quri stream))
        ((invalid-uri string _)
         (write-string string stream))
        ((data-uri string)
         (write-string string stream)))))

(defmethod parse-uri ((uri fake-response))
  (fake-uri uri))

(defmethod parse-uri ((uri null))
  null-uri)

(defmethod parse-uri ((uri uri))
  uri)

(defmethod parse-uri ((quri quri:uri))
  (valid-uri quri))

(defmethod parse-uri ((puri puri:uri))
  (with-accessors ((host     puri:uri-host)
                   (scheme   puri:uri-scheme)
                   (path     puri:uri-path)
                   (query    puri:uri-query)
                   (fragment puri:uri-fragment))
      puri
    (valid-uri
     (quri:make-uri
      :host host
      :scheme (if (keywordp scheme)
                  (string-downcase scheme)
                  scheme)
      :path path
      :query query
      :fragment fragment))))

(defun quri->puri (quri)
  (with-accessors ((host     quri:uri-host)
                   (scheme   quri:uri-scheme)
                   (path     quri:uri-path)
                   (query    quri:uri-query)
                   (fragment quri:uri-fragment))
      quri
    (make 'puri:uri
          :host host
          :scheme (if (stringp scheme)
                      (find-keyword (string-upcase scheme))
                      scheme)
          :path path
          :query query
          :fragment fragment)))

(defsubst latin-1? (char)
  (declare (optimize speed))
  (let ((code (char-code char)))
    (or (<= 32 code 126)
        (<= 160 code 255))))

(defun escape-parsed-path (path)
  (mapcar (lambda (part)
            (if (and (stringp part)
                     (notevery #'latin-1? part))
                (quri:url-encode part :encoding :utf-8)
                part))
          path))

(defun escape-query (query)
  (if (every #'latin-1? query) query
      (ppcre:regex-replace-all
       "[^&=]+" query
       (op (quri:url-encode _ :encoding :utf-8))
       :simple-calls t)))

(defun %parse-uri (uri)
  (etypecase-of (or string quri:uri) uri
    (quri:uri uri)
    (simple-string (quri:uri uri))
    (string (%parse-uri (coerce uri 'simple-string)))))

(defmethod parse-uri ((uri string))
  ;; Copy-paste errors.
  (let ((uri (string-trim "\" " uri)))
    (handler-case
        (cond ((string^= "data:" uri)
               (data-uri uri))
              ((in uri "None" "/None" "#")
               null-uri)
              (t (let ((uri (%parse-uri uri)))
                   (valid-uri
                    (quri:copy-uri uri
                                   :path (~> uri
                                             quri:uri-path
                                             parse-path
                                             escape-parsed-path
                                             unparse-path)
                                   :query (~> uri
                                              quri:uri-query
                                              escape-query)))
                   (valid-uri uri))))
      (invalid-uri-error (e)
        (invalid-uri (trim-whitespace uri) e)))))

(defun uri->string (uri)
  (match-uri uri
    (null-uri null-uri-string)
    ((valid-uri uri2)
     (quri:render-uri uri2 nil))
    ((invalid-uri string _)
     string)
    ((fake-uri fake) (uri->string (final-location fake)))
    ((data-uri string)
     string)))

(defun common-prefix? (x y)
  (cond ((emptyp x) nil)
        ((emptyp y) nil)
        (t (char= (aref x 0) (aref y 0)))))

(defmethod uri= ((uri1 string) (uri2 string))
  (or (string= uri1 uri2)
      (and (common-prefix? uri1 uri2)
           (call-next-method))))

(defmethod uri= (uri1 uri2)
  (let ((uri1 (parse-uri uri1))
        (uri2 (parse-uri uri2)))
    (or (equal (uri->string uri1)
               (uri->string uri2))
        (match-uri uri1
          (null-uri (eq uri2 null-uri))
          (fake-uri nil)
          ((invalid-uri string1 _)
           (match-uri uri2
             ((or null-uri valid-uri fake-uri data-uri) nil)
             ((invalid-uri string2 _)
              (equal string1 string2))))
          ((valid-uri quri1)
           (match-uri uri2
             ((or null-uri fake-uri invalid-uri data-uri) nil)
             ((valid-uri quri2)
              (quri:uri= quri1 quri2))))
          ((data-uri data1)
           (match-uri uri2
             ((or null-uri fake-uri invalid-uri valid-uri) nil)
             ((data-uri data2)
              (equal data1 data2))))))))

(defun invalid-uri? (uri)
  (match-uri uri
    (null-uri nil)
    (fake-uri t)
    (valid-uri nil)
    (invalid-uri t)
    (data-uri nil)))

(defun parse-valid-uri (uri)
  "A valid uri, or nil if URI could not be parsed."
  (let ((uri (parse-uri uri)))
    (if (invalid-uri? uri) nil uri)))

(defun merge-uris (uri base-uri)
  (match-uri uri
    ;; Null URI means no change either way.
    (null-uri base-uri)
    ;; Fake URIs cannot be merged.
    (fake-uri uri)
    ;; A valid URI overrides an invalid URI.
    (invalid-uri
     (match-uri base-uri
       ((or valid-uri fake-uri data-uri) base-uri)
       ((or null-uri invalid-uri) uri))
     base-uri)
    ((valid-uri quri)
     (match-uri base-uri
       ((or null-uri fake-uri invalid-uri data-uri) uri)
       ((valid-uri base-quri)
        (valid-uri (quri:merge-uris quri base-quri)))))
    (data-uri uri)))

(defun enough-uri (uri base)
  "Inverse of merge-uris."
  (flet ((remove-prefix (prefix string)
           (if (string^= prefix string)
               (uri (subseq string (length prefix)))
               (uri string))))
    (match-uri uri
      ((invalid-uri uri)
       (match-uri base
         ((valid-uri base)
          (remove-prefix (quri:render-uri base nil) uri))
         (_ uri)))
      ((valid-uri uri)
       (match-uri base
         ((valid-uri base)
          (puri:enough-uri (quri->puri uri)
                           (quri->puri base)))
         (_ uri)))
      (_ uri))))

;;; A fake URI is neither relative nor absolute.

(defun uri-relative? (uri)
  (match-uri uri
    (null-uri t)
    (fake-uri nil)
    ((valid-uri quri)
     (null (quri:uri-scheme quri)))
    (invalid-uri nil)
    (data-uri nil)))

(defun uri-absolute? (uri)
  (match-uri uri
    ((or null-uri fake-uri data-uri) nil)
    ((valid-uri quri)
     (and (quri:uri-scheme quri)
          (quri:uri-host quri)))
    (invalid-uri (uri-scheme uri))))

(defun uri-host-uri (uri)
  "The URI sans path."
  (match-uri uri
    ((or null-uri fake-uri) uri)
    ;; Can't analyze an invalid URI.
    ((or invalid-uri data-uri) null-uri)
    ((valid-uri quri)
     (quri:make-uri
      :scheme (quri:uri-scheme quri)
      :host   (quri:uri-host quri)
      :port   (quri:uri-port quri)))))

(defun extract-domain (string)
  (match string
    ((ppcre "^[a-z]+://([a-zA-Z](?:[a-zA-Z.-]*[a-zA-Z])?)" domain)
     domain)))

(defgeneric uri-domain (uri)
  (:method ((uri t))
    (match-uri uri
      ((valid-uri quri)
       (quri:uri-host quri))
      ((invalid-uri string)
       (extract-domain string))
      (_ nil)))
  ;; `uri-domain' is called a lot and has to be fast. We can save a
  ;; lot of time and consing if we extract the domain directly from
  ;; the string when possible.
  (:method ((uri string))
    (handler-case
        (let ((host (nth-value 2 (quri:parse-uri uri))))
          (if (stringp host) host (call-next-method)))
      (serious-condition ()
        (call-next-method)))))

(defun uri-host (uri)
  (uri-domain uri))

(defun ensure-https (uri)
  (uri->https uri))

(defun uri->https (uri)
  (match-uri uri
    ((or null-uri fake-uri invalid-uri data-uri) uri)
    ((valid-uri quri)
     (if (equal https (quri:uri-scheme quri)) uri
         (~> quri
             (quri:copy-uri
              :scheme https
              :port 443)
             valid-uri)))))

(defun string-strip-utm (string)
  (ppcre:regex-replace-all "(utm_[^&]*&?)" string ""))

(defun strip-utm (uri)
  "Remove any utm_ query parameters from URI."
  (let ((uri (parse-uri uri)))
    (match-uri uri
      ((or null-uri fake-uri data-uri) uri)
      ((invalid-uri string error)
       (invalid-uri (string-strip-utm string) error))
      ((valid-uri quri)
       (let ((query (quri:uri-query quri)))
         (if (null query) uri
             (valid-uri
              (let ((query2 (string-strip-utm query)))
                (quri:copy-uri quri
                               :query (if (emptyp query2) nil query2))))))))))

(defun canonicalize-uri (uri)
  (assure uri (strip-utm uri)))

(defun parse-feed-uri (uri)
  "Like PARSE-URI, but handles the feed:// protocol."
  (canonicalize-uri
   (ematch (uri->string uri)
     ((ppcre "^feed://(.*)" rest)
      (parse-uri (concat http "://" rest)))
     ((ppcre "^feed:(.*)" uri)
      (parse-uri uri))
     ((and uri (type string))
      (parse-uri uri)))))

(defun uri-scheme (uri)
  (assure (or null keyword)
    (match-uri uri
      ((or null-uri fake-uri) nil)
      (data-uri :data)
      ((invalid-uri string _)
       (when-let (match (nth-value 1 (ppcre:scan-to-strings "^(\\w+):" string)))
         (find-keyword (string-upcase (aref match 0)))))
      ((valid-uri uri)
       (let ((scheme (quri:uri-scheme uri)))
         (if (null scheme) nil
             (string-case scheme
               ("http" :http)
               ("https" :https)
               ("ftp" :ftp)
               ("ldap" :ldap)
               ("ldaps" :ldaps)
               ("file" :file)
               (t (find-keyword (string-upcase scheme))))))))))

(defun uri-path (uri)
  (match-uri uri
    ((valid-uri uri) (quri:uri-path uri))
    (_ nil)))

(defun scheme-port (scheme)
  (ecase scheme
    (:http 80)
    (:https 443)
    (:ftp 21)
    (:telnet 23)))

(defun quri-uri<= (uri1 uri2)
  (labels ((tail? (tail list)
             (loop for tail2 on list
                     thereis (equal tail tail2)))
           (compare-schemes ()
             (let ((scheme1 (quri:uri-scheme uri1))
                   (scheme2 (quri:uri-scheme uri2)))
               (or (null scheme1) (eql scheme1 scheme2))))
           (split-host (host)
             (split-sequence #\. (string-downcase host)))
           (compare-hosts ()
             (let ((host1 (quri:uri-host uri1))
                   (host2 (quri:uri-host uri2)))
               (or (null host1)
                   (tail? (split-host host1) (split-host host2)))))
           (guess-port (uri)
             (scheme-port (or (quri:uri-scheme uri) :http)))
           (compare-ports ()
             (let ((port1 (quri:uri-port uri1))
                   (port2 (quri:uri-port uri2)))
               (or (null port1)
                   (let ((port2 (or port2 (guess-port uri2))))
                     (eql port1 port2)))))
           (compare-paths ()
             (let ((path1 (remove "" (parse-path (quri:uri-path uri1)) :test #'equal))
                   (path2 (remove "" (parse-path (quri:uri-path uri2)) :test #'equal)))
               (cond ((null path1))
                     ((null path2) nil)
                     (t (ecase-of path-head (car path2)
                          (:absolute
                           (ecase-of path-head (car path1)
                             (:absolute (starts-with-subseq (cdr path1) (cdr path2)
                                                            :test #'equal))
                             (:relative (tail? (cdr path1) (cdr path2)))))
                          (:relative
                           (ecase-of path-head (car path1)
                             ;; y/z & x/y/z
                             (:relative (or (tail? (cdr path1) (cdr path2))))
                             (:absolute (tail? (cdr path2) (cdr path1))))))))))
           (split-query (query)
             (mapcar (op (split-sequence #\= _))
                     (split-sequence #\& query)))
           (compare-queries ()
             (let ((query1 (quri:uri-query uri1))
                   (query2 (quri:uri-query uri2)))
               (cond ((null query1))
                     ((null query2) nil)
                     (t (let ((query1 (split-query query1))
                              (query2 (split-query query2)))
                          (subsetp query1 query2 :test #'equal))))))
           (compare-fragments ()
             (let ((f1 (quri:uri-fragment uri1))
                   (f2 (quri:uri-fragment uri2)))
               (or (null f1) (equal f1 f2)))))
    (and (compare-schemes)
         (compare-hosts)
         (compare-ports)
         (compare-paths)
         (compare-queries)
         (compare-fragments))))

;;; The arity-1 version would be pointless.
(macrolet ((compiler-macro/2 (fn)
             `(define-compiler-macro ,fn (&whole decline &environment env
                                                 uri1 uri2)
                (cond ((constantp uri1 env)
                       (list ',fn (list 'load-time-value (list 'parse-uri uri1)) uri2))
                      ((constantp uri2 env)
                       (list ',fn uri1 (list 'load-time-value (list 'parse-uri uri2))))
                      (t decline)))))
  (compiler-macro/2 uri=)
  (compiler-macro/2 merge-uris)
  (compiler-macro/2 uri<=))

(defun uri<= (uri1 uri2)
  "Is URI1 included in URI2?

To put it another way: if you merged URI2 and URI1, would URI1 be changed?"
  (or (uri= uri1 uri2)
      (match-uri uri1
        ((or null-uri invalid-uri fake-uri data-uri) t)
        ((valid-uri quri1)
         (match-uri uri2
           ((or null-uri invalid-uri fake-uri data-uri) nil)
           ((valid-uri quri2)
            (quri-uri<= quri1 quri2)))))))

(defun uri-fragment (uri)
  (match-uri uri
    ((valid-uri quri)
     (quri:uri-fragment quri))
    ((invalid-uri string)
     (when-let (end (position #\# string))
       (subseq string end)))
    (_ uri)))

(defun strip-hash (uri)
  (match-uri uri
    ((valid-uri quri)
     (if (null (quri:uri-fragment quri)) uri
         (~> quri
             (quri:copy-uri :fragment nil)
             valid-uri)))
    ((invalid-uri string e)
     (let ((end (position #\# string)))
       (if (no end) uri
           (invalid-uri (subseq string 0 end) e))))
    (_ uri)))

(defun uri-directory (uri)
  (let ((uri (parse-uri uri)))
    (match-uri uri
      ((valid-uri quri)
       (let ((path (quri:uri-path quri)))
         (if (no path) uri
             (let ((last-slash (position #\/ path :from-end t)))
               (if (no last-slash) uri
                   (let ((path (subseq path 0 last-slash)))
                     (valid-uri (quri:merge-uris
                                 (quri:uri (concat path "/"))
                                 quri))))))))
      (_ uri))))

(defmethod fset:compare ((x uri) (y uri))
  (fset:compare-lexicographically (uri->string x)
                                  (uri->string y)))

(defun extract-tld (uri)
  ;; No extra consing!
  "Return three values: the TLD of URI, its domain, and its
subdomain (if there is one)."
  (if-let (host (uri-domain uri))
    (let* ((tld (cl-tld:get-tld host))
           (domain-end (- (length host) (1+ (length tld)))))
      (if-let (dot (position #\. host :from-end t :end domain-end))
        (let* ((domain (subseq host (1+ dot) domain-end))
               (subdomain (subseq host 0 dot)))
          (values tld domain subdomain))
        (values tld (subseq host 0 domain-end) nil)))
    (values nil nil nil)))

(defun parse-path (path)
  (if (null path) nil
      (let ((path (~>> path
                       trim-whitespace
                       (split-sequence #\/))))
        (if (equal (car path) "")
            (cons :absolute (rest path))
            (cons :relative path)))))

(defun unparse-path (path)
  (if (null path) nil
      (with-output-to-string (s)
        (ecase-of path-head (first path)
          (:relative)
          (:absolute (write-char #\/ s)))
        (loop for (part . more?) on (rest path) do
          (write-string part s)
          (when more?
            (write-char #\/ s))))))
