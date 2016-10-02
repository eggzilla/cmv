{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Visualize detailed comparsions of Infernal Covariance Models
--   Datastructures and parsing of covariance models is provided by Biobase
--   Visualization is accomplished with diagrams-svg
--   For more information on Infernal consult <http://eddylab.org/infernal/>

module Main where

import Bio.CMCompareResult
import Bio.CMDraw
--import qualified Biobase.SElab.CM as CM
import Biobase.SElab.CM.Import     
import System.Console.CmdArgs
import Data.Either
import qualified Data.Either.Unwrap as E
import System.Directory

options :: Options
data Options = Options            
  { cmcompareResultFile :: CmcompareResultFile,
    modelsFile :: ModelsFile,
    modelDetail :: ModelDetail,
    modelLayout :: ModelLayout,
    maxWidth :: Double,              
    comparisonAlignment :: ComparisonAlignment,
    outputFormat :: String 
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
    maxWidth = (100:: Double) &= name "w" &= help "Set maximal width of result figure (Default: 100)", 
    comparisonAlignment = "model" &= name "a" &= help "Set layout of drawn models: model, comparison",
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)"
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
       let comparisons = rights cmcResultParsed
       alnInput <- readStockholm alignmentFile
       let outputName = diagramName "cmcv" outputFormat
       let modelNumber = length models
       let alns = if (isRight alnInput) then (map (\a -> Just a) (fromRight alnInput)) else (replicate modelNumber Nothing)
       if oneOutputFile
              then do
                printCM (E.fromRight outputName) svgsize (drawCMComparisons modelDetail models alns comparisons)
	      else do
	        let modelNames = map ((++"."++outputFormat) . T.unpack . CM._name) models alns
		let modelVis = drawSingleCMComparisons modelDetail models alns comparisonsHighlightParameters
                mapM_ (\(a,b) -> printCM a svgsize b) (zip modelNames modelVis)
     else do
       if modelFileExists then return () else putStrLn "Model file not found"
       if cmcFileExists then return () else putStrLn "Comparison file not found"


  
