{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Visualize detailed comparsions of HMMER3 models
--   For more information on Infernal consult <http://hmmer.org/>

module Main where

import Bio.HMMCompareResult
import Bio.HMMDraw
import qualified Bio.HMMParser as HM
import System.Console.CmdArgs
import Data.Either
import qualified Data.Either.Unwrap as E
import System.Directory
import Bio.StockholmParser

options :: Options
data Options = Options            
  { hmmCompareResultFile :: String,
    modelsFile :: String,
    alignmentFile :: String,
    modelDetail :: String,
    modelLayout :: String,
    emissionLayout :: String,
    alignmentEntries :: Int,
    maxWidth :: Double,
    comparisonAlignment :: String,
    outputFormat :: String 
  } deriving (Show,Data,Typeable)

options = Options
  { hmmCompareResultFile = def &= name "r" &= help "Path to HMMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file",
    alignmentFile = "" &= name "s" &= help "Path to stockholm alignment file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: simple, detailed",
    modelLayout = "flat" &= name "l" &= help "Set layout of drawn models: flat, tree",
    emissionLayout = "box" &= name "e" &= help "Set layout of drawn models: score, probability, box (Default: box)",
    alignmentEntries = (50 :: Int) &= name "n" &= help "Set cutoff for included stockholm alignment entries (Default: 50)",
    maxWidth = (100:: Double) &= name "w" &= help "Set maximal width of result figure (Default: 100)",
    comparisonAlignment = "model" &= name "a" &= help "Set layout of drawn models: model, comparison",
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)"                      
  } &= summary "HMMCV devel version" &= help "Florian Eggenhofer - 2013-2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  --Validate input
  modelFileExists <- doesFileExist modelsFile
  hmmCFileExists <- doesFileExist hmmCompareResultFile
  alnFileExists <- doesFileExist alignmentFile
  if (modelFileExists && hmmCFileExists && alnFileExists)
    then do
      models <- HM.readHMMER3 modelsFile
      alns <- readStockholm alignmentFile
      let rightAlns = map Just (E.fromRight alns)
      if (isRight models)
        then do
          let rightModels = E.fromRight models
          hmmCResultParsed <- readHMMCompareResult hmmCompareResultFile
          if (isRight hmmCResultParsed)
            then do
              let rightHMMCResultsParsed = E.fromRight hmmCResultParsed
              --let comparisonModelNames = getModelsNames rightHMMCResultsParsed
              --let comparisonsHighlightParameters = getComparisonsHighlightParameters rightModels rightHMMCResultsParsed
              -- modelDetail entryNumberCutoff emissiontype maxWidth hmms alns comparisonHighlights
              let outputName = diagramName "test" outputFormat
              --let renderedHMM = drawHMMComparison modelDetail alignmentEntries emissionLayout maxWidth rightModels rightAlns comparisonsHighlightParameters
              printHMM (E.fromRight outputName) svgsize (drawHMMComparison modelDetail alignmentEntries emissionLayout maxWidth rightModels rightAlns rightHMMCResultsParsed)
            else print (E.fromLeft hmmCResultParsed)
        else print (E.fromLeft models)
    else do
      if modelFileExists then return () else putStrLn "Model file not found"
      if hmmCFileExists then return () else putStrLn "Comparison file not found"


  
