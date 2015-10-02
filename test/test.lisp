(in-package #:cl-user)

(defpackage #:yuri.test
  (:use #:cl #:alexandria #:serapeum #:fiveam #:yuri))

(in-package #:yuri.test)

(def-suite yuri)

(in-suite yuri)

(defun run-tests ()
  (5am:run! 'yuri))

(test bad-decoding
  (is (typep (parse-uri "?width=100%") 'invalid-uri)))

(test non-latin-1-query
  (is (equal
       (uri->string
        (parse-uri "http://vnwhr.net/wp-content/plugins/pinwheel-slider/includes/timthumb.php?src=http://vnwhr.net/wp-content/uploads/2014/06/Chúng-tôi-và-gia-đình-ông-Hào.jpg&h=300&w=450"))
       "http://vnwhr.net/wp-content/plugins/pinwheel-slider/includes/timthumb.php?src=http%3A%2F%2Fvnwhr.net%2Fwp-content%2Fuploads%2F2014%2F06%2FCh%C3%BAng-t%C3%B4i-v%C3%A0-gia-%C4%91%C3%ACnh-%C3%B4ng-H%C3%A0o.jpg&h=300&w=450")))

(test invalid-uri
  ;;; It's suprisingly hard to come up with examples of invalid URIs.
  (is (invalid-uri? "1234:://///##&!/"))
  (is (null (parse-valid-uri "1234:://///##&!/"))))

(test uri=
  (is (uri= (uri-host-uri "http://example.com/~foo") "http://example.com")))

(test extract-domain
  (is (equal "example.com" (extract-domain "http://example.com/~foo"))))

(test uri->https
  (is (uri= (uri->https "http://example.com") "https://example.com")))

(test strip-utm
  ;; All utm_.
  (is
   (uri= (parse-uri "http://kikolani.com/blog-post-promotion-ultimate-guide")
         (strip-utm "http://kikolani.com/blog-post-promotion-ultimate-guide?utm_source=kikolani&utm_medium=320banner&utm_campaign=bpp")))
  ;; Mixed.
  (is
   (uri= (parse-uri "http://kikolani.com/blog-post-promotion-ultimate-guide?baz=quux&foo=bar")
         (strip-utm "http://kikolani.com/blog-post-promotion-ultimate-guide?utm_source=kikolani&baz=quux&utm_medium=320banner&utm_campaign=bpp&foo=bar")))
  ;; No query.
  (is
   (uri= (parse-uri "http://example.com")
         (strip-utm "http://example.com")))
  (is
   (uri= (parse-uri "http://kikolani.com/blog-post-promotion-ultimate-guide?baz=quux&foo=bar")
         (strip-utm "http://kikolani.com/blog-post-promotion-ultimate-guide?baz=quux&foo=bar")))
  ;; Odd number of params.
  (let ((uri "http://www.oxfordbibliographies.com/view/document/obo-9780199766567/obo-9780199766567-0095.xml?rskey=b0IyH9&result=2&q="))
    (is (uri= (puri:parse-uri uri) (strip-utm uri))))
  ;; Deformed URLs?
  (let ((uri "http://www.oxfordbibliographies.com/view/document/obo-9780199766567/obo-9780199766567-0095.xml?rskey=b0IyH9&result=2&q="))
    (is (equal uri (uri->string (strip-utm uri))))))

(test parse-feed-uri
  (is
   (uri= (parse-feed-uri "feed://www.misspandora.fr/feed/")
         "http://www.misspandora.fr/feed/"))

  (is
   (uri= (parse-feed-uri "feed:https://example.com/entries.atom")
         "https://example.com/entries.atom")))


(test uri<=
  (is (not (uri<= "http://example.com" "foo.com/example.com")))
  (is (not (uri<= "https://example.com" "http://example.com")))
  (is (not (uri<= "http://example.com" "https://example.com")))
  (is (uri<= "/feeds/import" "http://example.com/feeds/import/quux"))
  (is (uri<= "/feeds/import" "http://example.com/feeds/import/"))
  (is (uri<= "?alt=rss" "http://example.com/feed?alt=rss"))
  (is (uri<= "?alt=rss" "http://example.com/feed?x=y&alt=rss"))
  (is (not (uri<= "?x=y&alt=rss" "http://example.com/feed?alt=rss")))
  (is (not (uri<= "?x=y&alt=rss" "http://example.com/feed?alt=rss")))
  (is (uri<= "http://feedburner.com/" "http://feeds.feedburner.com/tedblog"))
  (is (not (uri<= "http://feedburner.com/" "example.com/feedburner.com")))
  (is (not (uri<= "feedburner.com" "http://feeds.feedburner.com/tedblog")))
  (is (uri<= "http://www.jondascola.com/blog/" "http://www.jondascola.com/blog/give-a-good-goddamn"))
  ;; TODO
  #+ () (is (uri<= ".txt" "foo.txt")))

(test merge-uris
  (is (uri= "/x/y/z.html" (merge-uris "z.html" (uri-directory "/x/y/index.html")))))




