{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Compression.ShannonFano
import Options.Generic

data Options w = Options
    { decompress :: w ::: Maybe FilePath <?> "Decompress with decode table."
    , input :: w ::: Maybe FilePath <?> "Input content file. If not specified will be read from stdin."
    } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  x <- unwrapRecord "Compress contents using the Shannon-Fano algorithm"
  print (x :: Options Unwrapped)
