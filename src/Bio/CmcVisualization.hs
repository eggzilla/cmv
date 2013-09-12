{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

-- | Visualize detailed comparsions of Infernal Covariance Models
--   Datastructures and parsing of covariance models is provided by Biobase
--   Visualization is accomplished with diagrams-svg
--   For more information on Infernal consult <http://meme.nbcr.net/meme/>


module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Control.Monad
    
import Biobase.Primary
import qualified Biobase.SElab.CM as CM
import Biobase.SElab.CM.Import

import System.Console.CmdArgs
import Text.Printf

data Options = Options            
  { cmcompareResultFile :: CmcompareResultFile,
    modelsFile :: ModelsFile
  } deriving (Show,Data,Typeable)

type CmcompareResultFile = String
type ModelsFile = String
             
data CmcompareResult = CmcompareResult           
  { model1Name :: String,
    model2Name :: String,
    linkscore1 :: Double,
    linkscore2 :: Double,
    linksequence  :: String,
    model1structure :: String,
    model2structure :: String,
    model1matchednodes :: String,
    model2matchednodes :: String
  } deriving (Show,Data,Typeable)

             
options = Options
  { cmcompareResultFile = def &= name "r" &= help "Path to CMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file"
  } &= summary "CMCV devel version" &= help "egg 2013" &= verbosity


main = do
  Options{..} <- cmdArgs options
  let a = modelsFile
  let b = cmcompareResultFile
  models <- fromFile a
  cmcresult <- fromFile b
  print models
  print cmcresult     
