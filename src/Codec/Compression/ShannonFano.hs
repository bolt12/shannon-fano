{-# LANGUAGE OverloadedStrings #-}

module Codec.Compression.ShannonFano where

import Codec.Compression.ShannonFano.Internal
import Control.Arrow
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
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
frequency = map (BSL.head &&& (fromEnum . BSL.length)) . BSL.group . BSL.fromStrict . BS.sort . BSL.toStrict

-- | Gives the probability table of all characters in a string.
probability ::
  -- | Input string
  Input ->
  -- | Resulting table
  Table Float
probability s =
  let table = frequency s
      total = fromIntegral . fromEnum . BSL.length $ s
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
    aux ([(x, _)], r) = (x, "0") : map (second (BSL.append "1")) (aux (split r))
    aux (l, [(y, _)]) = map (second (BSL.append "0")) (aux (split l)) ++ [(y, "1")]
    aux (l, r) =
      let l2 = aux $ split l
          r2 = aux $ split r
       in map (second (BSL.append "0")) l2 ++ map (second (BSL.append "1")) r2

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
      | BSL.null s = BSL.empty
      | otherwise =
        let (x, xs) = (BSL.head &&& BSL.tail) s
            (Just r) = lookup x t
         in BSL.append r (aux xs t)

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
  | BSL.null s = Just BSL.empty
  | BSL.null (decompressWithLeftover s) = Just BSL.empty
  | otherwise =
    let decomps = decompressWithLeftover s
        (x, xs) = (BSL.head &&& BSL.tail) decomps
     in aux (map (snd &&& fst) t) xs (BSL.singleton x)
  where
    aux :: [(ByteString, Word8)] -> ByteString -> ByteString -> Maybe ByteString
    aux dt ls l =
      if BSL.null ls
        then case lookup l dt of
          Nothing -> Just ""
          Just r -> BSL.cons <$> Just r <*> Just ""
        else
          let (h, t) = (BSL.head &&& BSL.tail) ls
           in case lookup l dt of
                Nothing -> aux dt t (BSL.append l (BSL.singleton h))
                (Just r) -> BSL.cons <$> Just r <*> aux dt t (BSL.singleton h)

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
  contents <- BSL.hGetContents h
  let compressed = compress contents
      decodeTable = genCodeTable contents
  writeFile (filename ++ ".tab") (show decodeTable)
  BSL.writeFile filename compressed

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
  contents <- BSL.hGetContents h
  let decoded = decompress contents dt
  case decoded of
    Nothing -> return . Left $ DecodeTableError
    Just r -> Right <$> BSL.writeFile filename r
