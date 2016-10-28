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

import Bio.CMCompareResult
import Bio.CMDraw
import qualified Biobase.SElab.CM as CM
import Biobase.SElab.CM.Import     
import System.Console.CmdArgs
import System.Directory
import Data.Either.Unwrap
import qualified Bio.StockholmParser as SP
import qualified Data.Text as T
import qualified Data.Vector as V
import Paths_cmcv (version)
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
    outputFormat :: String,
    secondaryStructureVisualisation :: String,
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
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)",
    secondaryStructureVisualisation = "" &= name "x" &= help "Select tool for secondary structure visualisation: forna, r2r (Default: none)",
    oneOutputFile = False  &= name "o" &= help "Merge all output into one file (Default: False)"
  } &= summary ("cmv " ++ toolVersion) &= help "Florian Eggenhofer - 2013-2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  modelFileExists <- doesFileExist modelFile
  --alnFileExists <- doesFileExist alignmentFile
  if modelFileExists
    then do
      models <- fromFile modelFile
      if (not (null models))
        then do
          alnInput <- SP.readExistingStockholm alignmentFile
          if (isLeft alnInput) then print (fromLeft alnInput) else return ()
          let outputName = diagramName "cmv" outputFormat
	  let modelNumber = length models
          let alns = if (isRight alnInput) then (map (\a -> Just a) (fromRight alnInput)) else (replicate modelNumber Nothing)
	  if oneOutputFile
            then do	      
              printCM (fromRight outputName) svgsize (drawCMs modelDetail alignmentEntries modelLayout emissionLayout maxWidth models alns)
	    else do
	      let modelNames = map ((++ "." ++outputFormat) . T.unpack . CM._name) models
	      let modelVis = drawSingleCMs modelDetail alignmentEntries modelLayout emissionLayout maxWidth models alns
              mapM_ (\(a,b) -> printCM a svgsize b) (zip modelNames modelVis)
        else do
          print "Could not read covariance models from input file"
    else do
      putStrLn "Input model file not found"

toolVersion :: String
toolVersion = showVersion version
  
