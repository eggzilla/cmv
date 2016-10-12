{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Visualize detailed comparsions of Infernal Covariance Models
--   Datastructures and parsing of covariance models is provided by Biobase
--   Visualization is accomplished with diagrams-svg
--   For more information on Infernal consult <http://eddylab.org/infernal/>

module Main where

import Bio.CMCompareResult
import Bio.CMDraw
import qualified Biobase.SElab.CM as CM
import Biobase.SElab.CM.Import     
import System.Console.CmdArgs
import Data.Either
import qualified Data.Either.Unwrap as E
import System.Directory
import qualified Data.Text as T
import Bio.StockholmParser

options :: Options
data Options = Options            
  { cmcompareResultFile :: String,
    modelsFile :: String,
    alignmentFile :: String, 
    modelDetail :: String,
    modelLayout :: String,
    emissionLayout :: String,
    alignmentEntries :: Int,
    maxWidth :: Double,              
    comparisonAlignment :: String,
    outputFormat :: String,
    oneOutputFile :: Bool
  } deriving (Show,Data,Typeable)

options = Options
  { cmcompareResultFile = def &= name "r" &= help "Path to CMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file",
    alignmentFile = "" &= name "s" &= help "Path to stockholm alignment file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: minimal, simple, detailed",
    modelLayout = "flat" &= name "l" &= help "Set layout of drawn models: flat, tree",
    emissionLayout = "box" &= name "e" &= help "Set layout of drawn models: score, probability, box (Default: box)",
    alignmentEntries = (50 :: Int) &= name "n" &= help "Set cutoff for included stockholm alignment entries (Default: 50)",
    maxWidth = (100:: Double) &= name "w" &= help "Set maximal width of result figure (Default: 100)", 
    comparisonAlignment = "model" &= name "a" &= help "Set layout of drawn models: model, comparison",
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)",
    oneOutputFile = False  &= name "o" &= help "Merge all output into one file (Default: False)"
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
       let alns = if (isRight alnInput) then (map (\a -> Just a) (E.fromRight alnInput)) else (replicate modelNumber Nothing)
       if oneOutputFile
              then do
                printCM (E.fromRight outputName) svgsize (drawCMComparisons modelDetail alignmentEntries modelLayout emissionLayout maxWidth models alns comparisons)
              else do
                let modelNames = map ((++"."++outputFormat) . T.unpack . CM._name) models 
                let modelVis = drawSingleCMComparisons modelDetail alignmentEntries modelLayout emissionLayout maxWidth models alns comparisons
                mapM_ (\(a,b) -> printCM a svgsize b) (zip modelNames modelVis)
     else do
       if modelFileExists then return () else putStrLn "Model file not found"
       if cmcFileExists then return () else putStrLn "Comparison file not found"


  
