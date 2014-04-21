{-# Language BangPatterns #-}

module Histo (
              readSalida 
             ) where


import Control.Concurrent.Async
import Control.Lens
import Control.Monad 
import qualified Data.ByteString.Char8 as C
import Data.List as DL
import Data.List.Split (chunksOf)
import qualified Data.Vector.Unboxed as VU
import Text.Parsec
import Text.Parsec.ByteString (parseFromFile)

-- Internal modules 

-- =======> <===========
import APIparser
import CommonTypes
import Constants
import ParsecNumbers
import ParsecText

-- ===========> <============

readSalida :: FilePath -> IO [Double]
readSalida file = do
    r <- parseFromFile parseGaps file
    case r of
         Left  msg -> error . show $ msg
         Right xs  -> return xs
         
parseGaps :: MyParser () [Double]
parseGaps = many1 parseEnergies

parseEnergies :: MyParser () Double
parseEnergies = do
           count 2 anyLine
           manyTill anyChar (char ':')        
           spaces
           (e0:e1:_) <- count 2 (spaces >> realNumber )
           count 2 anyLine
           let gap = (e0 - e1) * ehKcal
           return gap
         