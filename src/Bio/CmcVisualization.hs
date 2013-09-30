{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

-- | Visualize detailed comparsions of Infernal Covariance Models
--   Datastructures and parsing of covariance models is provided by Biobase
--   Visualization is accomplished with diagrams-svg
--   For more information on Infernal consult <http://meme.nbcr.net/meme/>

module Main where

import CmcompareResult
import CmDraw
import Control.Monad    
import Biobase.Primary
import qualified Biobase.SElab.CM as CM
import qualified Data.Map as Map
import Biobase.SElab.CM.Import     
import System.Console.CmdArgs
import Text.Printf
 
data Options = Options            
  { cmcompareResultFile :: CmcompareResultFile,
    modelsFile :: ModelsFile,
    modelDetail :: ModelDetail
  } deriving (Show,Data,Typeable)

type CmcompareResultFile = String
type ModelsFile = String
type ModelDetail = String

options = Options
  { cmcompareResultFile = def &= name "r" &= help "Path to CMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: simple, detailed, tree"
  } &= summary "CMCV devel version" &= help "Florian Eggenhofer - 2013" &= verbosity

processCMs :: [CM.CM] -> [[(String,String)]]
processCMs cms = map processCMGuideTree cms

processCMGuideTree :: CM.CM -> [(String,String)]
processCMGuideTree cm = map getNodeInfo (Map.assocs (CM._nodes cm))

getNodeInfo :: (CM.NodeID, (CM.NodeType, [CM.StateID])) -> (String,String)
getNodeInfo (nodeid, (nodetype, _ )) = (show (CM.unNodeID nodeid) , (show nodetype))

main = do
  Options{..} <- cmdArgs options
  let a = modelsFile
  let b = cmcompareResultFile
  let c = modelDetail 
  models <- fromFile a
  cmResultParsed <- getCmcompareResults b              
  printSVG svgsize (drawCMGuideForest c (processCMs models))

  
