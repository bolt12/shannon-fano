{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Codec.Compression.ShannonFano as SF
import Options.Generic
import System.IO

data Options w = Options
    { decompress :: w ::: Maybe FilePath <?> "Decompress with decode table file name."
    , input :: w ::: Maybe FilePath <?> "Input content file name. If not specified will be read from stdin."
    , output :: w ::: Maybe FilePath <?> "Output result file name. If not specified will be 'out.bin' or 'out.dat' depending on the action"
    } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  x <- unwrapRecord "Compress contents using the Shannon-Fano algorithm"
  (out, fH) <- case (input x, output x, decompress x) of
    (Nothing      , Nothing , Nothing) -> return ("out.bin", stdin)
    (Nothing      , Nothing , Just _)  -> return ("out.dat", stdin)
    (Nothing      , Just out, _)       -> return (out      , stdin)
    (Just filename, Nothing , Nothing) -> ("out.bin", ) <$> openFile filename ReadMode
    (Just filename, Nothing , Just _)  -> ("out.dat", ) <$> openFile filename ReadMode
    (Just filename, Just out, _)       -> (out      , ) <$> openFile filename ReadMode
  case decompress x of
    Nothing        -> SF.compressToFile fH out
    Just tableFile -> do
      table <- read <$> readFile tableFile
      r <- SF.decompressFromFile fH table out
      case r of
        Left e -> print e >> hClose fH
        _ -> return ()
  print (x :: Options Unwrapped)
