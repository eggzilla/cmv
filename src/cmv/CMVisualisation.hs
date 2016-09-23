{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

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
import Bio.StockholmParser

options :: Options
data Options = Options            
  { modelFile :: String,
    alignmentFile :: String, 
    modelDetail :: String,
    modelLayout :: String
  } deriving (Show,Data,Typeable)


options = Options
  { modelFile = def &= name "m" &= help "Path to covariance model file",
    alignmentFile = "" &= name "s" &= help "Path to stockholm alignment file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: simple, detailed",
    modelLayout = "flat" &= name "l" &= help "Set layout of drawn models: flat, tree"
  } &= summary "CMV devel version" &= help "Florian Eggenhofer - 2013-2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  modelFileExists <- doesFileExist modelFile
  alnFileExists <- doesFileExist alignmentFile
  if modelFileExists
     then do
       model <- fromFile modelFile
       if not (null model)
          then do
            alnInput <- readStockholm alignmentFile
            let aln = if (isRight alnInput) then (Just (head (fromRight alnInput))) else Nothing
            printSVG svgsize (drawCMGuideForest modelDetail (processCMs (model)))    
          else 
            print "Could not read covariance models from input file"
     else do
       putStrLn "Input model file not found"




