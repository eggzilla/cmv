{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Visualize detailed comparsions of Infernal Covariance Models
--   Datastructures and parsing of covariance models is provided by Biobase
--   Visualization is accomplished with diagrams-svg
--   For more information on Infernal consult <http://eddylab.org/infernal/>

module Main where

import CmcompareResult
import CmDraw
import qualified Biobase.SElab.CM as CM
import Biobase.SElab.CM.Import     
import System.Console.CmdArgs
import Data.Either
import System.Directory

options :: Options
data Options = Options            
  { cmcompareResultFile :: CmcompareResultFile,
    modelsFile :: ModelsFile,
    modelDetail :: ModelDetail,
    modelLayout :: ModelLayout,
    comparisonAlignment :: ComparisonAlignment
  } deriving (Show,Data,Typeable)

type CmcompareResultFile = String
type ModelsFile = String
type ModelDetail = String
type ModelLayout = String
type ComparisonAlignment = String

options = Options
  { cmcompareResultFile = def &= name "r" &= help "Path to CMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: simple, detailed",
    modelLayout = "flat" &= name "l" &= help "Set layout of drawn models: flat, tree",
    comparisonAlignment = "model" &= name "a" &= help "Set layout of drawn models: model, comparison"
  } &= summary "CMCV devel version" &= help "Florian Eggenhofer - 2013-2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  --Validate input
  modelFileExists <- doesFileExist modelsFile
  cmcFileExists <- doesFileExist cmcompareResultFile
  if modelFileExists && cmcFileExists
     then do
       models <- fromFile modelsFile
       cmcResultParsed <- getCmcompareResults cmcompareResultFile
       checkCMCResultsParsed (lefts cmcResultParsed)
       let rightcmcResultsParsed = rights cmcResultParsed
       let comparisonModelNames = getModelsNames rightcmcResultsParsed
       --print (T.unpack (CM._name (models !!0)))
       --print (T.unpack (CM._name (models !!1)))
       --print rightcmcResultsParsed
       print comparisonModelNames
       --let findbtest = findModel "d" models
       --print findbtest
       --print models 
       --let sortedModels = sortModelsByComparisonResults comparisonModelNames models
       --print "Begin sorted Models:\n"
       --checkSortedModels (lefts sortedModels)
       --let rightSortedModels = (rights sortedModels)
       --print sortedModels
       --printSVG svgsize (drawCMGuideForest c (processCMs (rightSortedModels)) (getComparisonsHighlightParameters rightSortedModels rightcmcResultsParsed))
       let comparisonsHighlightParameters = getComparisonsHighlightParameters models rightcmcResultsParsed
       print comparisonsHighlightParameters
       printSVG svgsize (drawCMGuideForestComparison modelDetail (processCMs (models)) (comparisonsHighlightParameters)) 
     else do
       if modelFileExists then return () else putStrLn "Model file not found"
       if cmcFileExists then return () else putStrLn "Comparison file not found"


  
