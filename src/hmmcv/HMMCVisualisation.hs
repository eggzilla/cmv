{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Visualize detailed comparsions of HMMER3 models
--   For more information on Infernal consult <http://hmmer.org/>

module Main where

import Bio.HMMCompareResult
import Bio.HMMDraw
import System.Console.CmdArgs
import Data.Either
import System.Directory

options :: Options
data Options = Options            
  { hmmCompareResultFile :: String,
    modelsFile :: String,
    modelDetail :: String,
    modelLayout :: String,
    comparisonAlignment :: String
  } deriving (Show,Data,Typeable)

options = Options
  { hmmCompareResultFile = def &= name "r" &= help "Path to HMMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: simple, detailed",
    modelLayout = "flat" &= name "l" &= help "Set layout of drawn models: flat, tree",
    comparisonAlignment = "model" &= name "a" &= help "Set layout of drawn models: model, comparison"
  } &= summary "HMMCV devel version" &= help "Florian Eggenhofer - 2013-2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  --Validate input
  modelFileExists <- doesFileExist modelsFile
  cmcFileExists <- doesFileExist cmcompareResultFile
  if modelFileExists && cmcFileExists
     then do
       models <- fromFile modelsFile
       cmcResultParsed <- getHMMCompareResults hmmCompareResultFile
       checkHMMResultsParsed (lefts hmmcResultParsed)
       let righthmmcResultsParsed = rights hmmcResultParsed
       let comparisonModelNames = getModelsNames righthmmcResultsParsed
       let comparisonsHighlightParameters = getComparisonsHighlightParameters models righthmmcResultsParsed
       print comparisonsHighlightParameters
       printSVG svgsize (drawHMMComparison modelDetail models) comparisonsHighlightParameters
     else do
       if modelFileExists then return () else putStrLn "Model file not found"
       if cmcFileExists then return () else putStrLn "Comparison file not found"


  
