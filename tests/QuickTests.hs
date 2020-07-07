import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString()
import Percent.Encoder
import System.Exit
import qualified Data.ByteString as BS

prop_encodedecode :: BS.ByteString -> Bool
prop_encodedecode bs = decode (encode bs) == id bs

main :: IO ()
main = do
  result <- quickCheckResult prop_encodedecode
  unless (isSuccess result) exitFailure
