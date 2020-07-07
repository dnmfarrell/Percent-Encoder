Percent.Encoder
===============

A simple, fast percent encoder/decoder for bytestrings. Encodes all bytes except for those matching the unreserved set in [RFC 3986](https://tools.ietf.org/html/rfc3986).

Example
-------

    ghci> import qualified Data.ByteString.Char8 as C
    ghci> import Percent.Encoder
    ghci> encode $ C.pack "I have to tell you about this simple lib!"
    "I%20have%20to%20tell%20you%20about%20this%20simple%20lib%21"
    ghci> decode $ C.pack "TELL-ME-MORE%20%3A%29"
    "TELL-ME-MORE :)"

Gotchas
-------
* Does not check for invalid UTF-8 byte sequences

Author
------
© 2020 David Farrell, see LICENSE
