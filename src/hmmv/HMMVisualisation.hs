{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Visualize Hidden Markov Models

module Main where

import qualified Bio.HMMParser as HM
import Bio.HMMDraw
import System.Console.CmdArgs
import Data.Either.Unwrap
import System.Directory
import Bio.StockholmParser

options :: Options
data Options = Options            
  { modelFile :: String,
    alignmentFile :: String,             
    modelDetail :: String,
    modelLayout :: String
  } deriving (Show,Data,Typeable)


options = Options
  { modelFile = def &= name "m" &= help "Path to hidden Markov model file",
    alignmentFile = "" &= name "s" &= help "Path to stockholm alignment file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: simple, detailed",
    modelLayout = "flat" &= name "l" &= help "Set layout of drawn models: flat, tree"
  } &= summary "HMMvisualisation devel version" &= help "Florian Eggenhofer - 2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  modelFileExists <- doesFileExist modelFile
  alnFileExists <- doesFileExist alignmentFile
  if modelFileExists
     then do
       model <- HM.readHMMER3 modelFile
       --print model
       if alnFileExists
          then do
            aln <- readStockholm alignmentFile
            if (isRight aln) then print (fromRight aln) else print (fromLeft aln)
            if (isRight model) then printSVG svgsize (drawHMMER3 modelDetail (fromRight model)) else print (fromLeft model)
          else do     
            if (isRight model) then printSVG svgsize (drawHMMER3 modelDetail (fromRight model)) else print (fromLeft model)
     else do
       putStrLn "Input model file not found"
