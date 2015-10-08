YURI is a library for safe URI handling. It does not parse URIs; the
parsing is done by [QURI][]. What YURI does – all it does – is wrap
URIs in an [algebraic data type][].

YURI is born of bitter experience. In early versions of [TBRSS][] the
largest source of run-time errors, by an order of magnitude, was URI
parsing. It soon became clear that writing handlers everywhere a URI
might be parsed was the wrong approach.

(At the time, I used [PURI][]. [QURI][] is much less finicky, and more
careful about what errors it signals, but the argument still stands.)

Instead, by wrapping URIs in an algebraic data type, it becomes
possible to safely write functions that parse and access URIs of
uncertain provenance without special precautions.

(Although YURI uses an algebraic data type internally, it is not
necessary to use pattern matching: ordinary accessors are provided.)

You parse a URI using `yuri:parse-uri`:

    (setq valid (yuri:parse-uri "http://example.com"))
    => (YURI:VALID-URI #<QURI.URI.HTTP:URI-HTTP http://example.com>)

    (setq invalid (yuri:parse-uri "http://example.com?á"))
    => (YURI:INVALID-URI "http://example.com?foóbar" #<QURI.ERROR:URI-MALFORMED-STRING>)

    (yuri:uri-scheme valid)
    => :http

    (yuri:uri-scheme invalid)
    => :http

    (yuri:uri->string valid)
    => "http://example.com"

    (yuri:uri->string invalid)
    => "http://example.com?á"

[TBRSS]: https://tbrss.com
[QURI]: https://github.com/fukamachi/quri
[algebraic data type]: https://bitbucket.org/tarballs_are_good/cl-algebraic-data-type
