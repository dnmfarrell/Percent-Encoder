{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString()
import qualified Percent.Encoder as PE
import qualified Data.ByteString as BS
import qualified Network.URI.Encode as NUE

arbitraryByteStringVector :: Int -> IO [BS.ByteString]
arbitraryByteStringVector n = generate (vectorOf n arbitrary)

-- https://www.stackbuilders.com/news/obverse-versus-reverse-benchmarking-in-haskell-with-criterion
clockIt :: a -> IO ()
clockIt it = do
  start <- getTime Monotonic
  _ <- evaluate it
  end <- getTime Monotonic
  fprint (timeSpecs % "\n") start end

encoder :: (BS.ByteString -> BS.ByteString) -> [BS.ByteString] -> [BS.ByteString]
encoder _ [] = []
encoder f (b:bs) = f b : (encoder f bs)

main :: IO ()
main = do
  putStr "Percent.Encoder.encode: "
  arbitraryByteStringVector 100000000 >>= clockIt . (encoder PE.encode)
  putStr "Network.URI.Encode.encodeByteString: "
  arbitraryByteStringVector 100000000 >>= clockIt . (encoder NUE.encodeByteString)
  putStr "Percent.Encoder.decode: "
  arbitraryByteStringVector 100000000 >>= clockIt . (encoder PE.decode)
  putStr "Network.URI.Encode.decodeByteString: "
  arbitraryByteStringVector 100000000 >>= clockIt . (encoder NUE.decodeByteString)
