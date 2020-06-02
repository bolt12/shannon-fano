module Codec.Compression.ShannonFano.Internal
    ( Input,
      Table,
      split,
      chunksOf,
      compressChunk,
      compress
    )
  where

import Control.Arrow ((&&&))
import Data.Word
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Simple table used as an intermediate structure
type Table a = [(Word8, a)]

-- | Simple type alias to distinguish when something is an input or is
-- a coded 'Bytestring'
type Input = ByteString

-- | Auxiliary split function.
--
-- This function splits a probabilities table in half where the sum of the
-- two halfs are as close as possible.
split :: Table Float -> (Table Float, Table Float)
split t = aux t []
  where
    aux [] l = (l, [])
    aux (x:xs) l | snd x + sum (map snd l) < sum (map snd xs) = aux xs (l ++ [x])
                 | otherwise = (l ++ [x], xs)

-- | Takes a 'Bytestring' of 0s and 1s with length 8 and converts it to
-- a single 'Word8'.
--
-- Example:
-- @
-- compressChunk "00000001" == 1
-- @
compressChunk :: ByteString -> Word8
compressChunk s = aux s zeroBits
  where
    aux :: ByteString -> Word8 -> Word8
    aux s w | BS.null s = w
            | otherwise = let (h, (t, n)) = (BS.head &&& BS.tail &&& BS.length) s
                           in case h of
                                49 -> aux t (setBit w (n-1)) -- "1"
                                48 -> aux t w                -- "0"

-- | Creates a list of 'Bytestring's chunks.
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n = go
  where
    go t = case BS.splitAt n t of
               (a,b) | BS.null a -> []
                     | otherwise -> a : go b

-- | Takes a full 'Bytestring' of 0s and 1s and compresses it in binary
-- form. This way, a 'ByteString' that represents a binary string is
-- converted in its compressed form, occupying only the necessary bits.
compress :: ByteString -> ByteString
compress = BS.pack . map compressChunk . chunksOf 8

