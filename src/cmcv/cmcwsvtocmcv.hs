{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Visualize detailed comparsions of Infernal Covariance Models
--   Datastructures and parsing of covariance models is provided by Biobase
--   Visualization is accomplished with diagrams-svg
--   For more information on Infernal consult <http://eddylab.org/infernal/>

module Main where

import Bio.CMCompareResult
import qualified Biobase.SElab.CM as CM
import Biobase.SElab.CM.Import (cmFromFile)
import System.Console.CmdArgs
import Data.Either
import System.Directory
import qualified Data.Text as T
import Paths_cmv (version)
import Data.Version (showVersion)

options :: Options
data Options = Options
  { cmcompareResultFile :: String,
    modelsFile :: String
  } deriving (Show,Data,Typeable)

options = Options
  { cmcompareResultFile = def &= name "r" &= help "Path to CMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file"
  } &= summary ("cmcwstocmcv " ++ toolVersion ++ "Converts CMcompare webserver output and used model file to comparison file useable by cmcv") &= help "Florian Eggenhofer - 2017" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  --Validate input
  modelFileExists <- doesFileExist modelsFile
  cmcFileExists <- doesFileExist cmcompareResultFile
  if modelFileExists && cmcFileExists
     then do
       cms <- cmFromFile modelsFile
       cmcResultParsed <- getCmcompareResults cmcompareResultFile
       let comparisons = rights cmcResultParsed
       let currentModelNames = map (T.unpack . CM._name) cms
       let nameTuples = makeNameTuple currentModelNames
       let zippednameTuplesComparisons = zip nameTuples comparisons
       let namedComparisons = concatMap makeNamedComparison zippednameTuplesComparisons
       putStr namedComparisons
     else print "Input files not found\n"

makeNamedComparison :: ((String,String),CmcompareResult) -> String       
makeNamedComparison ((name1,name2),comparison) = show (comparison { model1Name = name1, model2Name = name2 })

makeNameTuple :: [String] -> [(String,String)]
makeNameTuple [] = []
makeNameTuple (mname:mnames)
  | null mnames = []
  | otherwise = ntuples ++ makeNameTuple mnames
    where ntuples = map (\a -> (mname,a)) mnames
toolVersion :: String
toolVersion = showVersion version
