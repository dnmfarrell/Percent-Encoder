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
My naive benchmark shows percent-encoder to be about 2x faster than
Network.URI at encoding and 25% slower at decoding a fixed string:

    $ cabal bench criterion
    ...

    benchmarking Percent.Encoder/encode
    
    time                 931.8 ns   (929.8 ns .. 933.7 ns)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 928.5 ns   (925.5 ns .. 931.2 ns)
    std dev              9.793 ns   (8.217 ns .. 11.97 ns)
    
    benchmarking Percent.Encoder/decode
    time                 714.9 ns   (712.3 ns .. 716.6 ns)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 711.1 ns   (709.8 ns .. 712.4 ns)
    std dev              4.518 ns   (3.835 ns .. 5.507 ns)
    
    benchmarking Network.URI/escapeURIString
    time                 1.856 μs   (1.854 μs .. 1.859 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 1.852 μs   (1.849 μs .. 1.855 μs)
    std dev              10.01 ns   (8.285 ns .. 12.62 ns)
    
    benchmarking Network.URI/unEscapeString
    time                 540.3 ns   (539.7 ns .. 540.7 ns)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 540.1 ns   (539.6 ns .. 540.6 ns)
    std dev              1.677 ns   (1.340 ns .. 2.289 ns)

Gotchas
-------
* Does not check for invalid UTF-8 byte sequences

Author
------
© 2020 David Farrell, see LICENSE
