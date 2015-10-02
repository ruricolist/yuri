YURI is a meta-library for parsing URIs. That is, it wraps other
URI-parsing libraries in such a way that `yuri:parse-uri` can never
result in a run-time error.

YURI is born of bitter experience. The URI standard is very flexible,
but difficult as it is people never fail to produce invalid inputs.
While writing early versions of [TBRSS][] the largest source of
runtime errors, by several orders of magnitude, was from trying to
parse invalid URIs with [PURI][].

In soon became clear that error handlers were fundamentally the wrong
approach. Instead of signals and handlers, YURI is built around an
[algebraic data type][]. The API is rich enough, however, that the end
user should not generally notice the difference.

[TBRSS]: https://tbrss.com
[QURI]: https://github.com/fukamachi/quri
[algebraic data type]: https://en.wikipedia.org/wiki/Algebraic_data_type
