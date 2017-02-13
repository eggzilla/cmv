{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Visualize Hidden Markov Models

module Main where

import qualified Bio.HMMParser as HM
import Bio.HMMDraw
import System.Console.CmdArgs
import Data.Either.Unwrap
import System.Directory
import qualified Bio.StockholmParser as SP
import Paths_cmv (version)
import Data.Version (showVersion)
import Data.List (intercalate)

options :: Options
data Options = Options            
  { modelFile :: String,
    alignmentFile :: String,             
    modelDetail :: String,
    emissionLayout :: String,
    alignmentEntries :: Int,
    maxWidth :: Double,
    scalingFactor :: Double,
    outputFormat :: String,
    outputDirectoryPath :: String,
    modelNameToggle :: Bool
  } deriving (Show,Data,Typeable)


options = Options
  { modelFile = def &= name "m" &= help "Path to hidden Markov model file",
    alignmentFile = "" &= name "s" &= help "Path to stockholm alignment file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: flat, simple, detailed",
    emissionLayout = "box" &= name "e" &= help "Set layout of drawn models: score, probability, box (Default: box)",
    alignmentEntries = (50 :: Int) &= name "n" &= help "Set cutoff for included stockholm alignment entries (Default: 50)",
    maxWidth = (200:: Double) &= name "w" &= help "Set maximal width of result figure (Default: 200)",
    scalingFactor = (2 :: Double) &= name "t" &= help "Set uniform scaling factor for image size (Default: 2)",
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)",
    outputDirectoryPath = "" &= name "p" &= help "Output directory path (Default: none)",
    modelNameToggle = False  &= name "b" &= help "Write all comma separted model names to modelNames file (Default: False)" 
  } &= summary ("hmmv " ++ toolVersion) &= help "Florian Eggenhofer - 2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  modelFileExists <- doesFileExist modelFile
  if modelFileExists
     then do
       inputModels <- HM.readHMMER3 modelFile
       if isRight inputModels
         then do
           alnInput <- SP.readExistingStockholm alignmentFile
           if (isLeft alnInput) then print (fromLeft alnInput) else return ()
           -- cairo outputFormat check
           let outputName = diagramName "hmmv" outputFormat
           let currentModels = fromRight inputModels
           let modelNumber = length currentModels
           let alns = if (isRight alnInput) then (map (\a -> Just a) (fromRight alnInput)) else (replicate modelNumber Nothing)
	   let currentModelNames = map HM.name currentModels
           currentWD <- getCurrentDirectory
           let dirPath = if null outputDirectoryPath then currentWD else outputDirectoryPath
	   setCurrentDirectory dirPath
           let modelFileNames = map (\m -> m ++ "." ++ outputFormat) currentModelNames
           let alignmentFileNames = map (\m -> m ++ ".aln" ++ "." ++ outputFormat) currentModelNames
           writeModelNameFile modelNameToggle outputDirectoryPath currentModelNames
           let (modelVis,alignmentVis) = unzip $ drawSingleHMMER3s modelDetail alignmentEntries maxWidth scalingFactor emissionLayout currentModels alns
           mapM_ (\(a,b) -> printHMM a svgsize b) (zip modelFileNames modelVis)
           mapM_ (\(a,b) -> printHMM a svgsize b) (zip alignmentFileNames alignmentVis)
           setCurrentDirectory currentWD 
         else 
           print (fromLeft inputModels)
     else do
       putStrLn "Input model file not found"

toolVersion :: String
toolVersion = showVersion version

writeModelNameFile :: Bool -> String -> [String] -> IO ()
writeModelNameFile toggle outputDirectoryPath modelNames = do
  if toggle
    then do
       let modelNamesString = intercalate "," modelNames
       writeFile (outputDirectoryPath ++ "modelNames") modelNamesString
    else return ()
