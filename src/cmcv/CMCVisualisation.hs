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
import qualified Bio.StockholmParser as SP
import Paths_cmcv (version)
import Data.Version (showVersion)

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
    secondaryStructureVisTool :: String,
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
    maxWidth = (200 :: Double) &= name "w" &= help "Set maximal width of result figure (Default: 100)", 
    comparisonAlignment = "model" &= name "a" &= help "Set layout of drawn models: model, comparison",
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)",
    secondaryStructureVisTool = "" &= name "x" &= help "Select tool for secondary structure visualisation: forna, r2r (Default: none)",
    oneOutputFile = False  &= name "o" &= help "Merge all output into one file (Default: False)"
  } &= summary ("cmcv " ++ toolVersion) &= help "Florian Eggenhofer - 2013-2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  --Validate input
  modelFileExists <- doesFileExist modelsFile
  cmcFileExists <- doesFileExist cmcompareResultFile
  if modelFileExists && cmcFileExists
     then do
       cms <- fromFile modelsFile
       cmcResultParsed <- getCmcompareResults cmcompareResultFile
       let comparisons = rights cmcResultParsed
       alnInput <- SP.readExistingStockholm alignmentFile
       if (isLeft alnInput) then print (E.fromLeft alnInput) else return ()
       let outputName = diagramName "cmcv" outputFormat
       let modelNumber = length cms
       let alns = if (isRight alnInput) then (map (\a -> Just a) (E.fromRight alnInput)) else (replicate modelNumber Nothing)
       let structureVisInputs = secondaryStructureVisualisation secondaryStructureVisTool maxWidth cms alns comparisons
       let modelNames = map ((++"."++outputFormat) . T.unpack . CM._name) cms
       if oneOutputFile
              then do
                printCM (E.fromRight outputName) svgsize (drawCMComparisons modelDetail alignmentEntries modelLayout emissionLayout maxWidth cms alns comparisons)
		mapM_ (\(a,b) -> writeFile (a ++ "." ++ secondaryStructureVisTool) b) (zip modelNames structureVisInputs)
              else do
                let modelVis = drawSingleCMComparisons modelDetail alignmentEntries modelLayout emissionLayout maxWidth cms alns comparisons
                mapM_ (\(a,b) -> printCM a svgsize b) (zip modelNames modelVis)
		mapM_ (\(a,b) -> writeFile (a ++ "." ++ secondaryStructureVisTool) b) (zip modelNames structureVisInputs)
     else do
       if modelFileExists then return () else putStrLn "Model file not found"
       if cmcFileExists then return () else putStrLn "Comparison file not found"

toolVersion :: String
toolVersion = showVersion version
