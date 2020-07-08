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
main = defaultMain [bgroup "" [
  bgroup "Percent.Encoder" [
    bench "encode" $ nf PE.encode uri,
    bench "decode" $ nf PE.decode encodedUri
  ],
  bgroup "Network.URI.Encode" [
    bench "encodeByteString" $ nf NUE.encodeByteString uri,
    bench "decodeByteString" $ nf NUE.decodeByteString encodedUri
  ]]]
