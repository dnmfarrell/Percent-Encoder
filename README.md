Percent.Encoder
===============

A simple, fast percent encoder/decoder for bytestrings. Encodes all bytes
except for those matching the unreserved set in
[RFC 3986](https://tools.ietf.org/html/rfc3986).

Example
-------

    ghci> :set -XOverloadedStrings
    ghci> import Percent.Encoder
    ghci> encode "I have to tell you about this simple lib!"
    "I%20have%20to%20tell%20you%20about%20this%20simple%20lib%21"
    ghci> decode "TELL-ME-MORE%20%3A%29"
    "TELL-ME-MORE :)"

Benchmark
---------
My naive benchmark shows percent-encoder, compared to Network.URI
to be about 2x faster than at encoding and the same speed at decoding
a fixed string:

    $ cabal bench criterion
    ...
    benchmarking Percent.Encoder/encode
    time                 890.7 ns   (887.3 ns .. 893.7 ns)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 887.6 ns   (885.1 ns .. 889.9 ns)
    std dev              8.241 ns   (7.090 ns .. 10.00 ns)
    
    benchmarking Percent.Encoder/decode
    time                 553.6 ns   (551.9 ns .. 554.8 ns)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 551.1 ns   (549.8 ns .. 552.2 ns)
    std dev              4.142 ns   (3.401 ns .. 5.173 ns)
    
    benchmarking Network.URI/escapeURIString
    time                 1.598 μs   (1.593 μs .. 1.603 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 1.584 μs   (1.581 μs .. 1.589 μs)
    std dev              13.65 ns   (11.55 ns .. 15.90 ns)
    
    benchmarking Network.URI/unEscapeString
    time                 530.7 ns   (530.2 ns .. 531.2 ns)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 530.6 ns   (529.9 ns .. 531.2 ns)
    std dev              2.053 ns   (1.717 ns .. 2.464 ns)


Gotchas
-------
* Does not check for invalid UTF-8 byte sequences
* Does not protect against decoding invalid strings, like `"%"`

Author
------
© 2020 David Farrell, see LICENSE
