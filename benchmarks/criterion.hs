{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Data.ByteString
import qualified Network.URI.Encode as NUE
import qualified Percent.Encoder as PE

uri :: ByteString
uri = "/package/criterion-1.5.6.2/docs/Criterion-Main.html"

encodedUri :: ByteString
encodedUri = "%2Fpackage%2Fcriterion-1.5.6.2%2Fdocs%2FCriterion-Main.html"

main :: IO ()
main = defaultMain [bgroup "encode/decode" [
  bgroup "encode" [
    bench "PE" $ nf PE.encode uri,
    bench "NUE" $ nf NUE.encodeByteString uri
  ],
  bgroup "decode" [
    bench "PE" $ nf PE.decode encodedUri,
    bench "NUE" $ nf NUE.decodeByteString encodedUri
  ]]]
