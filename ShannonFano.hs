module ShannonFano (
                   frequency, 
                   probability, 
                   compress, 
                   compressToFile,
                   code,
                   readDecodeTable,
                   decode,
                   decompressFromFile
                   ) where

import Data.List (group, sort, sortBy)
import Data.List.Split (chunksOf)
import Data.Char (intToDigit, ord)
import Numeric (readInt, showIntAtBase)
import System.IO
import qualified Data.ByteString as BS

----- * Auxiliar data types
--

type Table a = [(Char, a)]

type Encoding a = [((Char, a), String)]

type CodeTable = [(Char, String)]

type DecodeTable = [(String, Char)]

type Binary = [Int]

----- * Auxiliar functions
split :: (a -> b) -> (a -> c) -> a -> (b, c)
split f g x = (f x, g x)

(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
f >< g = split (f . fst) (g . snd)

third :: (a, b, c) -> c
third (a, b, c) = c

divide :: (Ord a, Num a) => Encoding a -> (Encoding a, Encoding a)
divide [] = ([],[])
divide (h:t) = let index = divide' t [h] 1
               in (map (id >< (flip (++) "0"))) >< (map (id >< (flip (++) "1"))) $ splitAt index (h:t)
    where
        divide' [] _ i = i
        divide' (h:t) l i | (sum $ map (snd . fst) (h:l)) < (sum $ map (snd . fst) t) = divide' t (h:l) (i+1) 
                          | otherwise = i

encode :: Table a -> Encoding a
encode = map (split id (const ""))

code' :: (Num a, Ord a) => Encoding a -> Encoding a
code' a = let (l,r) = divide a
             in code'' l ++ code'' r
         where
             code'' []  = []
             code'' [x] = [x]
             code'' s   = code' s

genCodeTable :: (Num a, Ord a) => Encoding a -> CodeTable
genCodeTable = map (fst >< id)

genDecodeTable :: (Num a, Ord a) => Encoding a -> DecodeTable
genDecodeTable = map (split snd fst) . genCodeTable

string2dec :: (Num a) => String -> a
string2dec s = fst $ (readInt 2 c d s) !! 0
    where
        c x = x == '1' || x == '0'
        d x = if x == '1' then 1 else 0

dec2binStr :: (Integral a, Show a) => a -> String
dec2binStr a = showIntAtBase 2 intToDigit a ""

getFromBinary :: FilePath -> IO (Int, [Int])
getFromBinary f = do
            fH <- openBinaryFile f ReadMode
            r <- BS.hGetContents fH
            (l, words) <- return . split head tail . BS.unpack $ r
            hClose fH
            return (fromIntegral l, map fromIntegral words)

----- * ShannonFano functions

-- | Gives the frequency table of all characters in a string.
--
frequency :: String -- ^ Input string
          -> Table Int -- ^ Resulting table
frequency = sortBy cmp . map (split head length) . group . sort
    where
        cmp x y = if snd x < snd y then GT else LT

-- | Gives the probability table of all characters in a string.
--
probability :: String  -- ^ Input string
            -> Table Float -- ^ Resulting table
probability s = sortBy cmp . map (split head prob) . group . sort $ s
                where
                    totalLength = length s
                    cmp x y     = if snd x < snd y then GT else LT
                    prob x      = (fromIntegral $ length x) / (fromIntegral totalLength)

-- | Given a 'Table' encodes it by applying the Shannon-fano
--   algorithm.
--
code :: (Num a, Ord a) => Table a -- ^ Input table
     -> Encoding a -- ^ Resulting encoding
code = code' . encode

-- | Compresses a string by applying a table generator function
--   and uses it to create a string of 0's and 1's.
--
compress :: (Num a, Ord a) => (String -> Table a) -- ^ 'frequency' or 'probability' can be applied
         -> String -- ^ String to compress
         -> Maybe String -- ^ Compressed, 0's and 1's, resulting string
compress f s = let encoding  = code . f $ s
                   codeTable = genCodeTable encoding
                   in (go codeTable s)
               where
                   go e []    = Just ""
                   go e (h:t) = (++) <$> lookup h e <*> go e t

-- | Compresses a string to a file.
--   
--   Calls the 'compress' function on the input string and
--   writes the binary code in a file.
--
--   The resulting files are:
--    - 'out.bin' <- binary compressed file
--    - 'decode.dat' <- contains the decoding table of out.bin
compressToFile :: (Num a, Ord a) => (String -> Table a) -- ^ 'frequency' or 'probability' can be applied
               -> String -- ^ String to compress
               -> IO ()
compressToFile f s = let decodeTable = genDecodeTable . code . f $ s
                         in do
                             fH <- openBinaryFile "out.bin" WriteMode
                             result <- return $ compress f s
                             case result of
                                 Nothing -> return ()
                                 Just r  -> do
                                                writeFile "decode.dat" (show decodeTable)
                                                chunks <- return $ map string2dec . chunksOf 8 $ r
                                                chunksL <- return $ (fromIntegral (8 - length r `mod` 8 )) : chunks -- Adds the length of the final byte
                                                BS.hPut fH (BS.pack chunksL)
                                                hClose fH

-- | Reads a 'DecodeTable' from a file
--
readDecodeTable :: FilePath -- ^ File path for the decode table
                -> IO (Maybe DecodeTable) -- ^ Decode table
readDecodeTable fp = do
                    dtContent <- readFile fp
                    return . readDT $ dtContent
            where
                readDT :: String -> Maybe DecodeTable
                readDT = fmap fst . safeHead . reads
                safeHead []    = Nothing
                safeHead (h:t) = Just h
                    
-- | Decodes a 'String' (made out of 0's and 1's) given a 'DecodeTable'
--
decode :: DecodeTable -- ^ Decoding table
       -> String -- ^ Example "0110110111"
       -> Maybe String -- ^ Resulting string
decode _ "" = Nothing
decode dt (h:t) = decode' dt t [h]
    where
        decode' dt [] l     = do
            case (lookup l dt) of
                Nothing  -> Just ""
                (Just r) -> (:) <$> (Just r) <*> (Just "")
        decode' dt (h:t) l = do
            case (lookup l dt) of
                Nothing  -> decode' dt t (l++[h])
                (Just r) -> (:) <$> (Just r) <*> (decode' dt t [h])

-- | Decompresses a file given a decoding table file and a compressed
--   binary file.
--
--   If the resulting output file is empty ("") the default name is
--   "result.dat"
decompressFromFile :: FilePath -- ^ File holding the decoding table info
                   -> FilePath -- ^ File holding the compressed binary
                   -> String -- ^ Resulting file to output.
                   -> IO ()
decompressFromFile dtf bf rf = do
        rfH <- case rf of
                  ""        -> openFile "result.dat" WriteMode
                  otherwise -> openFile rf WriteMode
        dt <- readDecodeTable dtf
        case dt of
            Nothing -> hClose rfH
            Just r  -> do
                (l, words) <- getFromBinary bf
                binaryString <- return $ fixBinary (l,words)
                decompressed <- return $ decode r $ binaryString
                case decompressed of
                    Nothing -> hClose rfH
                    Just d  -> do
                        hPutStr rfH d
                        hClose rfH
    where
        fixBinary (l, r) = let b       = map dec2binStr r
                               paddedB = map pad8 b
                               in concat $ init paddedB ++ [(drop (fromIntegral l) $ last paddedB)]
        pad8 s = let rest = 8 - length s
                     in replicate rest '0' ++ s
