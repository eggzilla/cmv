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
import Biobase.SElab.CM.Import (cmFromFile)
import System.Console.CmdArgs
import Data.Either
import qualified Data.Either.Unwrap as E
import System.Directory
import qualified Data.Text as T
import qualified Bio.StockholmParser as SP
import Paths_cmv (version)
import Data.Version (showVersion)
import Data.List (intercalate)
import Control.Monad
import Data.Maybe

options :: Options
data Options = Options
  { cmcompareResultFile :: String,
    modelsFile :: String,
    alignmentFile :: String,
    layoutDirection :: String,
    modelDetail :: String,
    modelLayout :: String,
    emissionLayout :: String,
    alignmentEntries :: Int,
    transitionCutoff :: Double,
    maxWidth :: Double,
    scalingFactor :: Double,
    comparisonAlignment :: String,
    outputFormat :: String,
    outputDirectoryPath :: String,
    secondaryStructureVisTool :: String,
    modelNameToggle :: Bool
  } deriving (Show,Data,Typeable)

options = Options
  { cmcompareResultFile = def &= name "r" &= help "Path to CMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file",
    alignmentFile = "" &= name "s" &= help "Path to stockholm alignment file",
    layoutDirection = "vertical" &= name "g" &= help "Set in which direction the model is drawn: vertical, horizontal (Default: vertical)",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: minimal, simple, detailed",
    modelLayout = "tree" &= name "l" &= help "Set layout of drawn models: flat, tree",
    emissionLayout = "box" &= name "e" &= help "Set layout of drawn models: score, probability, box (Default: box)",
    alignmentEntries = (50 :: Int) &= name "n" &= help "Set cutoff for included stockholm alignment entries (Default: 50)",
    transitionCutoff = (0.01 :: Double) &= name "t" &= help "Minimal value for a transition probability to be displayed (Default: 0.01)",
    maxWidth = (200 :: Double) &= name "w" &= help "Set maximal width of result figure (Default: 100)",
    scalingFactor = (2 :: Double) &= name "c" &= help "Set uniform scaling factor for image size (Default: 2)",
    comparisonAlignment = "model" &= name "a" &= help "Set layout of drawn models: model, comparison",
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)",
    outputDirectoryPath = "" &= name "p" &= help "Output directory path (Default: none)",
    secondaryStructureVisTool = "" &= name "x" &= help "Select tool for secondary structure visualisation: forna, r2r (Default: none)",
    modelNameToggle = False  &= name "b" &= help "Write all comma separted model names to modelNames file (Default: False)"
  } &= summary ("CMCV " ++ toolVersion) &= help "Florian Eggenhofer - 2013-2017" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  --Validate input
  modelFileExists <- doesFileExist modelsFile
  cmcFileExists <- doesFileExist cmcompareResultFile
  if modelFileExists && cmcFileExists
     then do
       cms <- cmFromFile modelsFile
       cmcResultParsed <- getCmcompareResults cmcompareResultFile
       let comparisons = rights cmcResultParsed
       alnInput <- SP.readExistingStockholm alignmentFile
       Control.Monad.when (isLeft alnInput) $ print (E.fromLeft alnInput)
       --let outputName = diagramName "cmcv" outputFormat
       let modelNumber = length cms
       let alns = if isRight alnInput then map (\a -> Just a) (E.fromRight alnInput) else replicate modelNumber Nothing
       let currentModelNames = map (T.unpack . CM._name) cms
       currentWD <- getCurrentDirectory
       let dirPath = if null outputDirectoryPath then currentWD else outputDirectoryPath
       writeModelNameFile modelNameToggle dirPath currentModelNames
       let modelFileNames = map (\m -> m ++ "." ++ outputFormat) currentModelNames
       let alignmentFileNames = map (\m -> m ++ ".aln" ++ "." ++ outputFormat) currentModelNames
       setCurrentDirectory dirPath
       let (modelVis,alignmentVis) = unzip $ drawSingleCMComparisons layoutDirection modelDetail alignmentEntries transitionCutoff modelLayout emissionLayout maxWidth scalingFactor cms alns comparisons
       mapM_ (\(a,b) -> printCM a svgsize b) (zip modelFileNames modelVis)
       mapM_ (\(alnPath,stockholm) -> if isJust stockholm then printCM alnPath svgsize (fromJust stockholm) else return ()) (zip alignmentFileNames alignmentVis)
       let structureVisType = "perModel"
       if structureVisType == "perModel"
         then do
           let structureFilePath =  dirPath ++ "/"
           let structureVisInputs = perModelSecondaryStructureVisualisation secondaryStructureVisTool maxWidth structureFilePath cms alns comparisons
           mapM_ (\(structureFileName,structureVis) -> writeFile structureFileName structureVis) structureVisInputs
           setCurrentDirectory currentWD
         else do
           let structureFilePaths = map (\m -> dirPath ++ "/" ++ m ++ "." ++ secondaryStructureVisTool) currentModelNames
           let structureVisInputs = mergedSecondaryStructureVisualisation secondaryStructureVisTool maxWidth cms alns comparisons
           mapM_ (\(structureFileName,(structureVis,_)) -> writeFile structureFileName structureVis) (zip structureFilePaths structureVisInputs)
           mapM_ (\(structureFileName,(_,colorScheme)) -> writeFile (structureFileName ++"Color") colorScheme) (zip structureFilePaths structureVisInputs)
     else do
       Control.Monad.unless modelFileExists $ putStrLn "Model file not found"
       Control.Monad.unless cmcFileExists $ putStrLn "Comparison file not found"

toolVersion :: String
toolVersion = showVersion version

writeModelNameFile :: Bool -> String -> [String] -> IO ()
writeModelNameFile toggle outputDirectoryPath modelNames =
  if toggle
    then do
       let modelNamesString = intercalate "," modelNames
       writeFile (outputDirectoryPath ++ "modelNames") modelNamesString
    else return ()
