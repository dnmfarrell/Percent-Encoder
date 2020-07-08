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
My naive benchmark shows percent-encoder to be about 17x faster than
uri-encode at encoding and 1.9x faster at decoding a fixed string:

    $ cabal bench criterion
    Preprocessing library percent-encoder-0.0.0.0...
    Preprocessing benchmark 'criterion' for percent-encoder-0.0.0.0...
    Running 1 benchmarks...
    Benchmark criterion: RUNNING...
    benchmarking Percent.Encoder/encode
    time                 986.4 ns   (983.5 ns .. 988.3 ns)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 980.2 ns   (977.8 ns .. 982.7 ns)
    std dev              8.103 ns   (7.128 ns .. 9.347 ns)

    benchmarking Percent.Encoder/decode
    time                 708.8 ns   (707.9 ns .. 709.4 ns)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 707.5 ns   (706.4 ns .. 708.3 ns)
    std dev              3.157 ns   (2.272 ns .. 4.392 ns)

    benchmarking Network.URI.Encode/encodeByteString
    time                 16.87 μs   (16.85 μs .. 16.89 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 16.89 μs   (16.87 μs .. 16.93 μs)
    std dev              91.50 ns   (50.34 ns .. 168.6 ns)

    benchmarking Network.URI.Encode/decodeByteString
    time                 1.377 μs   (1.376 μs .. 1.379 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 1.380 μs   (1.379 μs .. 1.382 μs)
    std dev              5.499 ns   (4.424 ns .. 7.359 ns)

    Benchmark criterion: FINISH


Gotchas
-------
* Does not check for invalid UTF-8 byte sequences

Author
------
© 2020 David Farrell, see LICENSE
