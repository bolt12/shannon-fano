{-# LANGUAGE OverloadedStrings #-}

module Codec.Compression.ShannonFano.Internal
  ( Input,
    Table,
    split,
    chunksOf,
    decode,
    compressChunk,
    compressWithLeftover,
    decompressWithLeftover,
  )
where

import Control.Arrow ((&&&))
import Data.Bits
import Data.Bool (bool)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Word

-- | Simple table used as an intermediate structure
type Table a = [(Word8, a)]

-- | Simple type alias to distinguish when something is an input or is
-- a coded or compressed 'Bytestring'.
type Input = ByteString

-- | Auxiliary split function.
--
-- This function splits a probabilities table in half where the sum of the
-- two halfs are as close as possible.
split :: Table Float -> (Table Float, Table Float)
split t = aux t []
  where
    aux [] l = (l, [])
    aux (x : xs) l
      | snd x + sum (map snd l) < sum (map snd xs) = aux xs (l ++ [x])
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
    aux s w
      | BSL.null s = w
      | otherwise =
        let (h, (t, n)) = (BSL.head &&& BSL.tail &&& BSL.length) s
         in case h of
              49 -> aux t (setBit w (fromEnum n - 1)) -- "1"
              48 -> aux t w                 -- "0"

-- | Creates a list of 'Bytestring' chunks.
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n = go
  where
    go t = case BSL.splitAt (toEnum n) t of
      (a, b)
        | BSL.null a -> []
        | otherwise -> a : go b

-- | Takes a full 'Bytestring' of 0s and 1s and compresses it in binary
-- form. This way, a 'ByteString' that represents a binary string is
-- converted in its compressed form, occupying only the necessary bits.
compress :: ByteString -> ByteString
compress = BSL.pack . map compressChunk . chunksOf 8

-- | The same as 'compress' but adds the size of the last byte to the
-- beginning of the compressed 'ByteString', since it might occupy less
-- than 8 bits.
compressWithLeftover :: ByteString -> ByteString
compressWithLeftover s = BSL.append (int2compressedBS (fromEnum (BSL.length s) `mod` 8)) (compress s)
  where
    int2compressedBS :: Int -> ByteString
    int2compressedBS n
      -- If n > 128 signifies that it does not fit in a 'Word8'
      | n > 128 = error "excess length greater than 8"
      | otherwise = compress . bool2BS . bitList $ n

-- | Takes a compressed 'Bytestring' and converts it into a 'Bytestring' of
-- only 0s and 1s.
decode :: ByteString -> ByteString
decode = BSL.concatMap (bool2BS . bitList)

-- | Takes a compressed 'Bytestring' and converts it into a 'Bytestring' of
-- only 0s and 1s, truncating the last byte accordingly, using the
-- information of the last byte's size.
decompressWithLeftover :: ByteString -> ByteString
decompressWithLeftover = BSL.concat . aux . (tail &&& (fromEnum . compressChunk . head)) . chunksOf 8 . decode
  where
    aux :: ([ByteString], Int) -> [ByteString]
    aux ([], _) = []
    aux ([x], 0) = [x] -- If 0 then there is no need to truncate
    aux ([x], i) = [BSL.drop (toEnum (8 - i)) x]
    aux (x : xs, i) = x : aux (xs, i)

-- | Creates a list of 'Bool's out of a bit.
bitList :: Bits a => a -> [Bool]
bitList x = map (testBit x) [7, 6 .. 0]

-- | Creates a bit 'ByteString' (a 'ByteString' full of 0s and 1s).
bool2BS :: [Bool] -> ByteString
bool2BS = BSL.concat . map (bool "0" "1")
