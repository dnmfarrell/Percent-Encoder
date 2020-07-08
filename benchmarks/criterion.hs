{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Data.ByteString()
import Network.URI
import qualified Percent.Encoder as PE

escape :: String -> String
escape = escapeURIString isUnreserved

main :: IO ()
main = defaultMain [bgroup "" [
  bgroup "Percent.Encoder" [
    bench "encode" $ nf PE.encode "/package/criterion-1.5.6.2/docs/Criterion-Main.html",
    bench "decode" $ nf PE.decode "%2Fpackage%2Fcriterion-1.5.6.2%2Fdocs%2FCriterion-Main.html"
  ],
  bgroup "Network.URI" [
    bench "escapeURIString" $ nf escape "/package/criterion-1.5.6.2/docs/Criterion-Main.html",
    bench "unEscapeString" $ nf unEscapeString "%2Fpackage%2Fcriterion-1.5.6.2%2Fdocs%2FCriterion-Main.html"
  ]]]
