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
import Biobase.SElab.CM
import Biobase.SElab.CM.Import

import System.Console.CmdArgs
import System.Environment (getArgs)
import Text.Printf

data Options = Options            
  { cmcompareResultFile :: CmcompareResultFile,
    models :: [String]
  } deriving (Show,Data,Typeable)

type  CmcompareResultFile = String
             
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
  { cmcompareResultFile = def &= help "CMCompare Result string",
    models = def &= args -- &= help "path to exactly two covariance models"
  } &= summary "CMCV devel version" &= help "2013, egg" &= verbosity


main = do
  Options{..} <- cmdArgs options
  unless (length models == 2) $ do
    fail "give exactly two CMs"
  let [a,b] = models
  let c = cmcompareResultFile
  [theA] <- fromFile a
  [theB] <- fromFile b
  cmcresult <- fromFile c         
  print [theA]
  print [theB]
  print cmcresult     
