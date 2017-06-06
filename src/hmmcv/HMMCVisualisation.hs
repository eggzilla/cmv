{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

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
import qualified Bio.StockholmParser as SP
import Paths_cmv (version)
import Data.Version (showVersion)
import Data.List (intercalate)
import Control.Monad
import Data.Maybe

options :: Options
data Options = Options
  { hmmCompareResultFile :: String,
    modelsFile :: String,
    alignmentFile :: String,
    modelDetail :: String,
    emissionLayout :: String,
    alignmentEntries :: Int,
    maxWidth :: Double,
    scalingFactor :: Double,
    transitionCutoff :: Double,
    comparisonAlignment :: String,
    outputFormat :: String,
    outputDirectoryPath :: String,
    modelNameToggle :: Bool
  } deriving (Show,Data,Typeable)

options = Options
  { hmmCompareResultFile = def &= name "r" &= help "Path to HMMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file",
    alignmentFile = "" &= name "s" &= help "Path to stockholm alignment file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: minimal, simple, detailed",
    emissionLayout = "box" &= name "e" &= help "Set layout of drawn models: score, probability, box (Default: box)",
    alignmentEntries = (50 :: Int) &= name "n" &= help "Set cutoff for included stockholm alignment entries (Default: 50)",
    maxWidth = (100:: Double) &= name "w" &= help "Set maximal width of result figure (Default: 100)",
    scalingFactor = (2 :: Double) &= name "c" &= help "Set uniform scaling factor for image size (Default: 2)",
    transitionCutoff = (0.01 :: Double) &= name "t" &= help "Minimal value for a transition probability to be displayed (Default: 0.01)",
    comparisonAlignment = "model" &= name "a" &= help "Set layout of drawn models: model, comparison",
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)",
    outputDirectoryPath = "" &= name "p" &= help "Output directory path (Default: none)",
    modelNameToggle = False  &= name "b" &= help "Write all comma separted model names to modelNames file (Default: False)"
  } &= summary ("HMMCV " ++ toolVersion) &= help "Florian Eggenhofer - 2013-2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  --Validate input
  modelFileExists <- doesFileExist modelsFile
  hmmCFileExists <- doesFileExist hmmCompareResultFile
  alnFileExists <- doesFileExist alignmentFile
  if modelFileExists && hmmCFileExists && alnFileExists
    then do
      inputModels <- HM.readHMMER3 modelsFile
      if isRight inputModels
        then do
          let models = E.fromRight inputModels
          hmmCResultParsed <- readHMMCompareResult hmmCompareResultFile
          let modelNumber = length models
          alnInput <- SP.readExistingStockholm alignmentFile
          Control.Monad.when (isLeft alnInput) $ print (E.fromLeft alnInput)
          let alns = if isRight alnInput then map (\a -> Just a) (E.fromRight alnInput) else replicate modelNumber Nothing
          let currentModelNames = map HM.name models
          currentWD <- getCurrentDirectory
          let dirPath = if null outputDirectoryPath then currentWD else outputDirectoryPath
          if isRight hmmCResultParsed
            then do
              let rightHMMCResultsParsed = E.fromRight hmmCResultParsed
              --let outputName = diagramName "hmmcv" outputFormat
              setCurrentDirectory dirPath
              let modelFileNames = map (\m -> m ++ "." ++ outputFormat) currentModelNames
              let alignmentFileNames = map (\m -> m ++ ".aln" ++ "." ++ outputFormat) currentModelNames
              writeModelNameFile modelNameToggle dirPath currentModelNames
              let (modelVis,alignmentVis) = unzip $ drawSingleHMMComparison modelDetail alignmentEntries transitionCutoff emissionLayout maxWidth scalingFactor models alns rightHMMCResultsParsed
              mapM_ (\(a,b) -> printHMM a svgsize b) (zip modelFileNames modelVis)
              mapM_ (\(alnPath,stockholm) -> if isJust stockholm then printHMM alnPath svgsize (fromJust stockholm) else return ()) (zip alignmentFileNames alignmentVis)
              setCurrentDirectory currentWD
            else print (E.fromLeft hmmCResultParsed)
        else print (E.fromLeft inputModels)
    else do
      Control.Monad.unless modelFileExists $ putStrLn "Model file not found"
      Control.Monad.unless hmmCFileExists $ putStrLn "Comparison file not found"

toolVersion :: String
toolVersion = showVersion version

writeModelNameFile :: Bool -> String -> [String] -> IO ()
writeModelNameFile toggle outputDirectoryPath modelNames =
  if toggle
    then do
       let modelNamesString = intercalate "," modelNames
       writeFile (outputDirectoryPath ++ "modelNames") modelNamesString
    else return ()
