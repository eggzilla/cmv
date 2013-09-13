{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

-- | Visualize detailed comparsions of Infernal Covariance Models
--   Datastructures and parsing of covariance models is provided by Biobase
--   Visualization is accomplished with diagrams-svg
--   For more information on Infernal consult <http://meme.nbcr.net/meme/>

module Main where

import CmcompareResult    
import Diagrams.Prelude
import Diagrams.TwoD
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
                         
options = Options
  { cmcompareResultFile = def &= name "r" &= help "Path to CMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file"
  } &= summary "CMCV devel version" &= help "Florian Eggenhofer - 2013" &= verbosity

             

example = circle 0.5 <> unitCircle
               
main = do
  Options{..} <- cmdArgs options
  let a = modelsFile
  let b = cmcompareResultFile
  models <- fromFile a
  print a
  print b
  cmResultParsed <- getCmcompareResults b
  --print models
  print cmResultParsed  
