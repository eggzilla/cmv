{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Visualize Infernal Covariance Models
--   Datastructures and parsing of covariance models is provided by Biobase
--   Visualization is accomplished with diagrams-svg
--   For more information on Infernal consult <http://eddylab.org/infernal/>

module Main where

import Bio.CMDraw
import qualified Biobase.SElab.CM as CM
import Biobase.SElab.CM.Import     
import System.Console.CmdArgs
import System.Directory
import Data.Either.Unwrap
import qualified Bio.StockholmParser as SP
import qualified Data.Text as T
import Paths_cmv (version)
import Data.Version (showVersion)    

options :: Options
data Options = Options            
  { modelFile :: String,
    alignmentFile :: String, 
    modelDetail :: String,
    modelLayout :: String,
    emissionLayout :: String,
    alignmentEntries :: Int,
    maxWidth :: Double,
    scalingFactor :: Double,
    outputFormat :: String,
    outputDirectoryPath :: String,
    secondaryStructureVisTool :: String,
    oneOutputFile :: Bool
  } deriving (Show,Data,Typeable)

options = Options
  { modelFile = def &= name "m" &= help "Path to covariance model file",
    alignmentFile = "" &= name "s" &= help "Path to stockholm alignment file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: minimal, simple, detailed",
    modelLayout = "flat" &= name "l" &= help "Set layout of drawn models: flat, tree",
    emissionLayout = "box" &= name "e" &= help "Set layout of drawn models: score, probability, box (Default: box)",
    alignmentEntries = (50 :: Int) &= name "n" &= help "Set cutoff for included stockholm alignment entries (Default: 50)",
    maxWidth = (200 :: Double) &= name "w" &= help "Set maximal width of result figure (Default: 100)",
    scalingFactor = (2.0 :: Double) &= name "t" &= help "Set uniform scaling factor for image size (Default: 2)",
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)",
    outputDirectoryPath = "" &= name "p" &= help "Output directory path (Default: none)",
    secondaryStructureVisTool = "" &= name "x" &= help "Select tool for secondary structure visualisation: forna, r2r (Default: none)",
    oneOutputFile = False  &= name "o" &= help "Merge all output into one file (Default: False)"
  } &= summary ("cmv " ++ toolVersion) &= help "Florian Eggenhofer - 2013-2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  modelFileExists <- doesFileExist modelFile
  --alnFileExists <- doesFileExist alignmentFile
  if modelFileExists
    then do
      cms <- fromFile modelFile
      if (not (null cms))
        then do
          alnInput <- SP.readExistingStockholm alignmentFile
          if (isLeft alnInput) then print (fromLeft alnInput) else return ()
          let outputName = diagramName "cmv" outputFormat
          let modelNumber = length cms
          let alns = if (isRight alnInput) then (map (\a -> Just a) (fromRight alnInput)) else (replicate modelNumber Nothing)
          let structureVisInputs = secondaryStructureVisualisation secondaryStructureVisTool maxWidth cms alns []
          let modelFileNames = map ((++ "." ++ outputFormat) . T.unpack . CM._name) cms
          let structureFileNames = map ((++ "." ++ secondaryStructureVisTool) . T.unpack . CM._name) cms
          if oneOutputFile
            then do	      
              printCM (fromRight outputName) svgsize (drawCMs modelDetail alignmentEntries modelLayout emissionLayout maxWidth scalingFactor cms alns) 
              mapM_ (\(structureFileName,(structureVis,_)) -> writeFile structureFileName structureVis) (zip structureFileNames structureVisInputs)
              mapM_ (\(structureFileName,(_,colorScheme)) -> writeFile (structureFileName ++"Color") colorScheme) (zip structureFileNames structureVisInputs)
            else do
              let modelVis = drawSingleCMs modelDetail alignmentEntries modelLayout emissionLayout maxWidth scalingFactor cms alns
              mapM_ (\(a,b) -> printCM a svgsize b) (zip modelFileNames modelVis)
              mapM_ (\(structureFileName,(structureVis,_)) -> writeFile structureFileName structureVis) (zip structureFileNames structureVisInputs)
              mapM_ (\(structureFileName,(_,colorScheme)) -> writeFile (structureFileName ++"Color") colorScheme) (zip structureFileNames structureVisInputs)
        else do
          print "Could not read covariance models from input file"
    else do
      putStrLn "Input model file not found"

toolVersion :: String
toolVersion = showVersion version
  
