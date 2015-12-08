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
import Biobase.SElab.Types
import qualified Data.Map as Map
import Biobase.SElab.CM.Import     
import System.Console.CmdArgs
import Text.Printf
import Data.Maybe
import Data.List
import Data.Either
import qualified Data.ByteString.Char8 as BS
import Language.Haskell.TH.Ppr
import Text.Parsec.Error
import qualified Data.Text as T
import qualified Data.Vector as V

data Options = Options            
  { cmcompareResultFile :: CmcompareResultFile,
    modelsFile :: ModelsFile,
    modelDetail :: ModelDetail,
    modelLayout :: ModelLayout,
    comparisonAlignment :: ComparisonAlignment
  } deriving (Show,Data,Typeable)

type CmcompareResultFile = String
type ModelsFile = String
type ModelDetail = String
type ModelLayout = String
type ComparisonAlignment = String

options = Options
  { cmcompareResultFile = def &= name "r" &= help "Path to CMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: simple, detailed",
    modelLayout = "flat" &= name "l" &= help "Set layout of drawn models: flat, tree",
    comparisonAlignment = "model" &= name "a" &= help "Set layout of drawn models: model, comparison"
  } &= summary "CMCV devel version" &= help "Florian Eggenhofer - 2013" &= verbosity

processCMs :: [CM.CM] -> [[(String,String)]]
processCMs cms = map processCMGuideTree cms

processCMGuideTree :: CM.CM -> [(String,String)]
--processCMGuideTree cm = map getNodeInfo (Map.assocs (CM._nodes cm))
processCMGuideTree cm = map getNodeInfo (V.toList (CM._nodes cm))

--getNodeInfo :: (CM.Node, (CM.NodeType, [CM.State])) -> (String,String)
--getNodeInfo (nodeid, (nodetype, _ )) = (show (CM._nId nodeid) , (show nodetype))

getNodeInfo :: CM.Node -> (String,String)
getNodeInfo node = (show (CM._nid node) , show (CM._ntype node))

sortModelsByComparisonResults :: [String] -> [CM.CM] -> [Either String CM.CM]
sortModelsByComparisonResults cmComparisonNames models = map (\x -> findModelError x (findModel x models)) cmComparisonNames
-- todo: also add models at end of the sorted list that are not in comparisons
-- ++ (map (\x -> findModelError x (findMissing x models)) cmComparisonNames)

findModelError :: String -> Maybe CM.CM -> Either String CM.CM
findModelError name (Just model) = Right model
findModelError name Nothing = Left ("Model " ++ name ++ "that is present in comparison file is not present in model file")

findModel :: String -> [CM.CM] -> Maybe CM.CM
findModel check models = find (\x -> getCMName x == check) models

findModelIndex :: String -> [CM.CM] -> Maybe Int
findModelIndex check models = findIndex (\x -> getCMName x == check) models

findMissing :: String -> [CM.CM] -> Maybe CM.CM
findMissing check models = find (\x -> getCMName x /= check) models

getCMName :: CM.CM -> String
getCMName x = filter (\c -> c /= ' ')  (T.unpack (CM._name x))

checkCMCResultsParsed x 
  | null x = print "Parsing comparisons - done\n"
  | otherwise = error ("Following errors occured:" ++ (concat (map checkCmcResultParsed x)))

checkCmcResultParsed x = (concat (map messageString (errorMessages x)))
--  | (errorIsUnknown x) = print "Parsing comparisons - done\n" 
--  | x == [] = print "Parsing comparisons - done\n"
--  | otherwise = (concat (map messageString (errorMessages x)))
--  | otherwise = error ("Following errors occured :" ++ (concat (map (\y -> (concat (map messageString (errorMessages y)))) x)))

checkSortedModels x
  | x == [] = print "Sorting input models to comparison list - done\n" 
  | otherwise = error ("Following errors occured :" ++ show (concat x))

getComparisonsHighlightParameters :: [CM.CM] -> [CmcompareResult] -> [(Int,Int,Int,Int,Int,Int,Int,Int)]
getComparisonsHighlightParameters sortedmodels comp = map (getComparisonHighlightParameters sortedmodels) comp

getComparisonHighlightParameters :: [CM.CM] -> CmcompareResult -> (Int,Int,Int,Int,Int,Int,Int,Int)
getComparisonHighlightParameters sortedmodels comp = (a,b,c,d,a,f,c,e)
  where --a = (fromJust (findModelIndex (model1Name comp) sortedmodels) + 1)
        a = 1
        b = head (model1matchednodes comp)
        --c = (fromJust (findModelIndex (model2Name comp) sortedmodels) + 1)
        c = 2
        d = head (model2matchednodes comp)
        e = last (model2matchednodes comp)
        f = last (model1matchednodes comp)

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  let a = modelsFile
  let b = cmcompareResultFile
  let c = modelDetail 
  models <- fromFile a
  cmcResultParsed <- getCmcompareResults b              
  checkCMCResultsParsed (lefts cmcResultParsed)
  let rightcmcResultsParsed = rights cmcResultParsed
  let comparisonModelNames = getModelsNames rightcmcResultsParsed
  --print (T.unpack (CM._name (models !!0)))
  --print (T.unpack (CM._name (models !!1)))
  --print rightcmcResultsParsed
  print comparisonModelNames
  --let findbtest = findModel "d" models
  
  --print findbtest
  --print models 
--  let sortedModels = sortModelsByComparisonResults comparisonModelNames models
--  print "Begin sorted Models:\n"
--  checkSortedModels (lefts sortedModels)
--let rightSortedModels = (rights sortedModels)
--  print sortedModels
--printSVG svgsize (drawCMGuideForest c (processCMs (rightSortedModels)) (getComparisonsHighlightParameters rightSortedModels rightcmcResultsParsed))
  let comparisonsHighlightParameters = getComparisonsHighlightParameters models rightcmcResultsParsed
  print comparisonsHighlightParameters
  printSVG svgsize (drawCMGuideForest c (processCMs (models)) (comparisonsHighlightParameters))



  
