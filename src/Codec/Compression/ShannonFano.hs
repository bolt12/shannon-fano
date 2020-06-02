{-# LANGUAGE OverloadedStrings #-}

module Codec.Compression.ShannonFano where

import Codec.Compression.ShannonFano.Internal
import Control.Arrow
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (lookup, sortBy)
import Data.Word (Word8)
import System.IO

-- | Decode table error can happen when the wrong code table is provided.
data DecodeTableError = DecodeTableError
  deriving (Eq, Show)

-- | Gives the frequency table of all characters in a string.
frequency ::
  -- | Input string
  Input ->
  -- | Resulting table
  Table Int
frequency = map (BS.head &&& BS.length) . BS.group . BS.sort

-- | Gives the probability table of all characters in a string.
probability ::
  -- | Input string
  Input ->
  -- | Resulting table
  Table Float
probability s =
  let table = frequency s
      total = fromIntegral . BS.length $ s
   in map (second ((/ total) . fromIntegral)) table

-- | Generates a 'DecodeTable'
genCodeTable ::
  -- | Input string
  Input ->
  -- | Resulting code table
  Table ByteString
genCodeTable s =
  let table = sortBy cmp $ probability s
   in aux (split table)
  where
    cmp :: (Word8, Float) -> (Word8, Float) -> Ordering
    cmp x y = if snd x < snd y then GT else LT
    aux :: (Table Float, Table Float) -> Table ByteString
    aux ([], []) = []
    aux ([(x, _)], [(y, _)]) = [(x, "0"), (y, "1")]
    aux ([(x, _)], r) = (x, "0") : map (second (BS.append "1")) (aux (split r))
    aux (l, [(y, _)]) = map (second (BS.append "0")) (aux (split l)) ++ [(y, "1")]
    aux (l, r) =
      let l2 = aux $ split l
          r2 = aux $ split r
       in map (second (BS.append "0")) l2 ++ map (second (BS.append "1")) r2

-- | Given a 'Table ByteString' compresses it by applying the Shannon-fano
--   algorithm.
compress ::
  -- | Input string
  Input ->
  -- | Result compressed
  ByteString
compress s = compressWithLeftover $ aux s (genCodeTable s)
  where
    aux :: ByteString -> Table ByteString -> ByteString
    aux s t
      | BS.null s = BS.empty
      | otherwise =
        let (x, xs) = (BS.head &&& BS.tail) s
            (Just r) = lookup x t
         in BS.append r (aux xs t)

-- | Decompresses a compressed 'ByteString', given a code table
--
--   This fails if the code table does not have an entry for a given
--   character.
decompress ::
  -- | Coded input to decompress
  ByteString ->
  -- | Code table associated with the input
  Table ByteString ->
  -- | Result decompressed
  Maybe Input
decompress s t
  | BS.null s = Just BS.empty
  | BS.null (decompressWithLeftover s) = Just BS.empty
  | otherwise =
    let decomps = decompressWithLeftover s
        (x, xs) = (BS.head &&& BS.tail) decomps
     in aux (map (snd &&& fst) t) xs (BS.singleton x)
  where
    aux :: [(ByteString, Word8)] -> ByteString -> ByteString -> Maybe ByteString
    aux dt ls l =
      if BS.null ls
        then case lookup l dt of
          Nothing -> Just ""
          Just r -> BS.cons <$> Just r <*> Just ""
        else
          let (h, t) = (BS.head &&& BS.tail) ls
           in case lookup l dt of
                Nothing -> aux dt t (BS.append l (BS.singleton h))
                (Just r) -> BS.cons <$> Just r <*> aux dt t (BS.singleton h)

-- | Reads contents from a handle and compresses it to a file.
--
--   The resulting files are:
--    - '<filename>' <- binary compressed file
--    - '<filename>.dat' <- contains the decoding table
compressToFile ::
  -- | Handle from where data will be read
  Handle ->
  -- | Output file name
  String ->
  IO ()
compressToFile h filename = do
  contents <- BS.hGetContents h
  let compressed = compress contents
      decodeTable = genCodeTable contents
  writeFile (filename ++ ".dat") (show decodeTable)
  BS.writeFile filename compressed

-- | Decompresses a file given a decoding table file and a compressed
--   compressed file.
decompressFromFile ::
  -- | Handle from where compressed data will be read
  Handle ->
  -- | Decode table
  Table ByteString ->
  -- | Output file name
  String ->
  IO (Either DecodeTableError ())
decompressFromFile h dt filename = do
  contents <- BS.hGetContents h
  let decoded = decompress contents dt
  case decoded of
    Nothing -> return . Left $ DecodeTableError
    Just r -> Right <$> BS.writeFile filename r
