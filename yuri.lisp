;;;; yuri.lisp

(in-package #:yuri)

;;; "yuri" goes here. Hacks and glory await!

;; “All conditions serious enough to require interactive intervention
;; if not handled should inherit from the type ‘serious-condition’.”
(deftype invalid-uri-error ()
  'serious-condition)

(def null-uri-string "#")

(defgeneric parse-uri (source)
  (:documentation "Parse SOURCE into a URI."))

(defun uri (source)
  "Parse SOURCE into a URI."
  (parse-uri source))

(defclass fake-response ()
  ((body :initarg :body)
   (status-code :initarg :status-code :initarg :status :type (integer 0 599))
   (headers :initarg :headers :type list)
   (final-location :initarg :final-location :type (or string puri:uri)
                   :accessor final-location)
   (error :initarg :error))
  (:documentation "A fake URI for testing. When called with
  HTTP-REQUEST, returns the provided values.")
  (:default-initargs
   :body nil
   :status-code 200
   :headers '()
   :error nil
   :final-location (puri:parse-uri "http://example.com")))

(defmethod fake-response-values ((self fake-response))
  (with-slots (body status-code headers final-location error) self
    (if error
        (funcall error)
        (values body status-code headers final-location))))

(adt:defdata uri
  null-uri
  (valid-uri puri:uri)
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
  (if *print-escape*
      (call-next-method)
      (adt:match uri self
        (null-uri
         (write-string null-uri-string stream))
        (fake-uri
         (call-next-method))
        ((valid-uri puri)
         (print-object puri stream))
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

(defmethod parse-uri ((puri puri:uri))
  (valid-uri puri))

(defmethod parse-uri ((quri quri:uri))
  (with-accessors ((host     quri:uri-host)
                   (scheme   quri:uri-scheme)
                   (path     quri:uri-path)
                   (query    quri:uri-query)
                   (fragment quri:uri-fragment))
      quri
    (parse-uri
     (make 'puri:uri
           :host host
           :scheme scheme
           :path path
           :query query
           :fragment fragment))))

(defsubst latin-1? (char)
  (declare (optimize speed))
  (let ((code (char-code char)))
    (or (<= 32 code 126)
        (<= 160 code 255))))

(defun escape-puri-path (path)
  (mapcar (lambda (part)
            (if (and (stringp part)
                     (notevery #'latin-1? part))
                (quri:url-encode part :encoding :utf-8)
                part))
          path))

(defun escape-puri-query (query)
  (if (every #'latin-1? query)
      query
      (ppcre:regex-replace-all
       "[^&=]+" query
       (op (quri:url-encode _ :encoding :utf-8))
       :simple-calls t)))

(defmethod parse-uri ((uri string))
  ;; Copy-paste errors.
  (let ((uri (string-trim "\" " uri)))
    (handler-case
        (cond ((string^= "data:" uri)
               (data-uri uri))
              ((in uri "None" "/None" "#")
               null-uri)
              (t (let ((uri (puri:parse-uri uri)))
                   (when-let (path (puri:uri-parsed-path uri))
                     (setf (puri:uri-parsed-path uri)
                           (escape-puri-path (puri:uri-parsed-path uri))
                           (puri:uri-query uri)
                           (escape-puri-query (puri:uri-query uri))))
                   (valid-uri uri))))
      (invalid-uri-error (e)
        (invalid-uri (trim-whitespace (remove-if-not #'latin-1? uri)) e)))))

(defun uri->string (uri)
  (match-uri uri
    (null-uri null-uri-string)
    ((valid-uri uri2)
     (puri:render-uri uri2 nil))
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
          ((valid-uri puri1)
           (match-uri uri2
             ((or null-uri fake-uri invalid-uri data-uri) nil)
             ((valid-uri puri2)
              (puri:uri= puri1 puri2))))
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
  ;; E.g. http://mailto:alex@iphonelife.com
  (let ((uri (parse-uri uri)))
    (if (invalid-uri? uri)
        nil
        uri)))

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
    ((valid-uri puri)
     (match-uri base-uri
       ((or null-uri fake-uri invalid-uri data-uri) uri)
       ((valid-uri base-puri)
        (valid-uri (puri:merge-uris puri base-puri)))))
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
          (remove-prefix (puri:render-uri base nil) uri))
         (_ uri)))
      ((valid-uri uri)
       (match-uri base
         ((valid-uri base)
          (puri:enough-uri uri base))
         (_ uri)))
      (_ uri))))

;;; A fake URI is neither relative nor absolute.

(defun uri-relative? (uri)
  (match-uri uri
    (null-uri t)
    (fake-uri nil)
    ((valid-uri puri)
     (null (puri:uri-scheme puri)))
    (invalid-uri nil)
    (data-uri nil)))

(defun uri-absolute? (uri)
  (match-uri uri
    ((or null-uri fake-uri data-uri) nil)
    ((valid-uri puri)
     (and (puri:uri-scheme puri)
          (puri:uri-host puri)))
    (invalid-uri (uri-scheme uri))))

(defun uri-host-uri (uri)
  "The URI sans path."
  (match-uri uri
    ((or null-uri fake-uri) uri)
    ;; Can't analyze an invalid URI.
    ((or invalid-uri data-uri) null-uri)
    ((valid-uri puri)
     (valid-uri (puri:merge-uris "/" puri)))))

(defun extract-domain (string)
  (match string
    ((ppcre "^[a-z]+://([a-zA-Z](?:[a-zA-Z.-]*[a-zA-Z])?)" domain)
     domain)))

;; `uri-domain' is called a lot and has to be fast. We can save a lot
;; of time and consing if we extract the domain directly from the
;; string when possible.
(defgeneric uri-domain (uri)
  (:method ((uri t))
    (match-uri uri
      ((valid-uri puri)
       (puri:uri-host puri))
      ((invalid-uri string)
       (extract-domain string))
      (_ nil)))
  (:method ((uri string))
    (handler-case
        (let ((host (nth-value 2 (quri:parse-uri uri))))
          (if (stringp host)
              host
              (call-next-method)))
      (serious-condition ()
        (call-next-method)))))

(defun uri-host (uri)
  (uri-domain uri))

(defun ensure-https (uri)
  (uri->https uri))

(defun uri->https (uri)
  (match-uri uri
    ((or null-uri fake-uri invalid-uri data-uri) uri)
    ((valid-uri puri)
     (if (eql (puri:uri-scheme puri) :https)
         uri
         (let ((puri2 (puri:copy-uri puri)))
           (setf (puri:uri-scheme puri2) :https)
           (valid-uri puri2))))))

(defun string-strip-utm (string)
  (ppcre:regex-replace-all "(utm_[^&]*&?)" string ""))

(defun strip-utm (uri)
  (let ((uri (parse-uri uri)))
    (match-uri uri
      ((or null-uri fake-uri data-uri) uri)
      ((invalid-uri string error)
       (invalid-uri (string-strip-utm string) error))
      ((valid-uri puri)
       (let ((query (puri:uri-query puri)))
         (if (no query)
             uri
             (valid-uri
              (let ((query2 (string-strip-utm query)))
                (if (emptyp query2)
                    (puri:copy-uri puri :query nil)
                    (puri:copy-uri puri :query query2))))))))))

(defun canonicalize-uri (uri)
  (assure uri (strip-utm uri)))

(defun parse-feed-uri (uri)
  "Like PARSE-URI, but handles the feed:// protocol."
  (canonicalize-uri
   (ematch (uri->string uri)
     ((ppcre "^feed://(.*)" rest)
      (parse-uri (concat "http://" rest)))
     ((ppcre "^feed:(.*)" uri)
      (parse-uri uri))
     ((and uri (type string))
      (parse-uri uri)))))

(defun uri-scheme (uri)
  (match-uri uri
    ((or null-uri fake-uri) nil)
    (data-uri :data)
    ((invalid-uri string _)
     (when-let (match (nth-value 1 (ppcre:scan-to-strings "^(\\w+):" string)))
       (find-keyword (string-upcase (aref match 0)))))
    ((valid-uri uri)
     (puri:uri-scheme uri))))

(defun uri-path (uri)
  (match-uri uri
    ((valid-uri uri) (puri:uri-path uri))
    (_ nil)))

(defun scheme-port (scheme)
  (case scheme
    (:http 80)
    (:https 443)
    (:ftp 21)
    (:telnet 23)))

(defun puri-uri<= (uri1 uri2)
  (labels ((tail? (tail list)
             (loop for tail2 on list
                     thereis (equal tail tail2)))
           (compare-schemes ()
             (let ((scheme1 (puri:uri-scheme uri1))
                   (scheme2 (puri:uri-scheme uri2)))
               (or (null scheme1) (eql scheme1 scheme2))))
           (split-host (host)
             (split-sequence #\. (string-downcase host)))
           (compare-hosts ()
             (let ((host1 (puri:uri-host uri1))
                   (host2 (puri:uri-host uri2)))
               (or (null host1)
                   (tail? (split-host host1) (split-host host2)))))
           (guess-port (uri)
             (scheme-port (or (puri:uri-scheme uri) :http)))
           (compare-ports ()
             (let ((port1 (puri:uri-port uri1))
                   (port2 (puri:uri-port uri2)))
               (or (null port1)
                   (let ((port2 (or port2 (guess-port uri2))))
                     (eql port1 port2)))))
           (compare-paths ()
             (let ((path1 (remove "" (puri:uri-parsed-path uri1) :test #'equal))
                   (path2 (remove "" (puri:uri-parsed-path uri2) :test #'equal)))
               (cond ((null path1))
                     ((null path2) nil)
                     (t (ecase (car path2)
                          (:absolute
                           (ecase (car path1)
                             (:absolute (starts-with-subseq (cdr path1) (cdr path2)
                                                            :test #'equal))
                             (:relative (tail? (cdr path1) (cdr path2)))))
                          (:relative
                           (ecase (car path1)
                             ;; y/z & x/y/z
                             (:relative (or (tail? (cdr path1) (cdr path2))))
                             (:absolute (tail? (cdr path2) (cdr path1))))))))))
           (split-query (query)
             (mapcar (op (split-sequence #\= _))
                     (split-sequence #\& query)))
           (compare-queries ()
             (let ((query1 (puri:uri-query uri1))
                   (query2 (puri:uri-query uri2)))
               (cond ((null query1))
                     ((null query2) nil)
                     (t (let ((query1 (split-query query1))
                              (query2 (split-query query2)))
                          (subsetp query1 query2 :test #'equal))))))
           (compare-fragments ()
             (let ((f1 (puri:uri-fragment uri1))
                   (f2 (puri:uri-fragment uri2)))
               (or (null f1) (equal f1 f2)))))
    (and (compare-schemes)
         (compare-hosts)
         (compare-ports)
         (compare-paths)
         (compare-queries)
         (compare-fragments))))

(defun uri<= (uri1 uri2)
  "Is URI1 included in URI2?

To put it another way: if you merged URI2 and URI1, would URI1 be changed?"
  (or (uri= uri1 uri2)
      (match-uri uri1
        ((or null-uri invalid-uri fake-uri data-uri) t)
        ((valid-uri puri1)
         (match-uri uri2
           ((or null-uri invalid-uri fake-uri data-uri) nil)
           ((valid-uri puri2)
            (puri-uri<= puri1 puri2)))))))

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

(defun uri-fragment (uri)
  (match-uri uri
    ((valid-uri puri)
     (puri:uri-fragment puri))
    ((invalid-uri string)
     (when-let (end (position #\# string))
       (subseq string end)))
    (_ uri)))

(defun strip-hash (uri)
  (match-uri uri
    ((valid-uri puri)
     (if (no (puri:uri-fragment puri))
         uri
         (let ((puri2 (puri:copy-uri puri)))
           (setf (puri:uri-fragment puri2) nil)
           (valid-uri puri2))))
    ((invalid-uri string e)
     (let ((end (position #\# string)))
       (if (no end)
           uri
           (invalid-uri (subseq string 0 end) e))))
    (_ uri)))

(defun uri-directory (uri)
  (let ((uri (parse-uri uri)))
    (match-uri uri
      ((valid-uri puri)
       (let ((path (puri:uri-path puri)))
         (if (null path)
             uri
             (let ((last-slash (position #\/ path :from-end t)))
               (if (no last-slash)
                   uri
                   (let ((path (subseq path 0 last-slash)))
                     (if (emptyp path)
                         (valid-uri (puri:merge-uris "/" puri))
                         (valid-uri (puri:merge-uris (concat path "/") puri)))))))))
      (_ uri))))

(defmethod fset:compare ((x uri) (y uri))
  (let ((x (uri->string x)) (y (uri->string y)))
    (fset:compare-lexicographically x y)))

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
