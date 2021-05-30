#lang scribble/manual

@(require (for-label geoip
                     racket))

@title{GeoIP}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[geoip]


@section{Introduction}

This library provides utilities for working with MaxMind's geolocation
databases.  It lets you look up information about where an IP address
is located.


@section{Usage}

To use this library, you're going to need to @hyperlink["https://dev.maxmind.com/geoip/geoip2/geolite2/"]{download}
one of MaxMind's V2 databases.  The library has been tested against
their free "GeoLite2" databases, but should work with their enterprise
DBs as well.

Once you've downloaded one of the databases, you can read it using
@racket[make-geoip].  That'll return a structure that you can use to
perform lookups using @racket[geoip-lookup].  For example:

@racketblock[
  (define geoip (make-geoip "/path/to/some-database.mmdb"))
  (geoip-lookup geoip "127.0.0.1")
  (geoip-lookup geoip "188.24.7.80")
]


@section{Reference}

@defproc[(geoip? (x any/c)) boolean?]{
  Returns @racket[#t] when @racket[x] is a geolocation database.
}

@defproc[(make-geoip (path path-string?)) geoip?]{
  Reads a MaxMind geolocation database into memory.  Raises an error
  if the database is not a version 2 database or if it is otherwise
  invalid.
}

@defproc[(geoip-metadata [geoip geoip?]) (hash/c string? any/c)]{
  Returns the metadata for the given @racket[geoip] database.
}

@defproc[(geoip-lookup [geoip geoip?]
                       [ip string?]) (or/c false/c hash?)]{
  Looks up an @racket[ip] in the @racket[geoip] database and returns a
  hash of information about the address on success or @racket[#f] when
  the address cannot be found.
}
