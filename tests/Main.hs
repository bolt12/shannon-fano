module Main where

import Codec.Compression.ShannonFano
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack)
import Text.Printf (printf)
import Test.QuickCheck (Arbitrary(..), quickCheck)

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

prop_id :: ByteString -> Bool
prop_id s = let dt = genCodeTable s
             in ((`decompress` dt) $ compress s) == Just s

tests :: [(String, IO ())]
tests = [ ("prop_id", quickCheck prop_id)
        ]

main :: IO ()
main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
