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
    modelLayout :: String,
    emissionLayout :: String,
    alignmentEntries :: Int,
    maxWidth :: Double,
    outputFormat :: String
  } deriving (Show,Data,Typeable)


options = Options
  { modelFile = def &= name "m" &= help "Path to hidden Markov model file",
    alignmentFile = "" &= name "s" &= help "Path to stockholm alignment file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: simple, detailed",
    modelLayout = "detailed" &= name "l" &= help "Set layout of drawn models: flat, tree (Default: detailed)",
    emissionLayout = "box" &= name "e" &= help "Set layout of drawn models: score, probability, box (Default: box)",
    alignmentEntries = (50 :: Int) &= name "n" &= help "Set cutoff for included stockholm alignment entries (Default: 50)",
    maxWidth = (100:: Double) &= name "w" &= help "Set maximal width of result figure (Default: 100)",
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)"
  } &= summary "HMMvisualisation devel version" &= help "Florian Eggenhofer - 2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  modelFileExists <- doesFileExist modelFile
  alnFileExists <- doesFileExist alignmentFile
  if modelFileExists
     then do
       model <- HM.readHMMER3 modelFile
       if alnFileExists
          then do
            aln <- readStockholm alignmentFile
            --if (isRight model) then printSVG svgsize (drawHMMER3 modelDetail alignmentEntries maxWidth emissionLayout (head (fromRight model),(Just (head (fromRight aln))))) else print (fromLeft model)
	    if (isRight model) then printPDF svgsize (drawHMMER3 modelDetail alignmentEntries maxWidth emissionLayout (head (fromRight model),(Just (head (fromRight aln))))) else print (fromLeft model)
          else do     
            if (isRight model) then printSVG svgsize (drawHMMER3 modelDetail alignmentEntries maxWidth emissionLayout (head (fromRight model),Nothing)) else print (fromLeft model)
     else do
       putStrLn "Input model file not found"
