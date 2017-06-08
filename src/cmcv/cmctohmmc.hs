{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 

module Main where

import Bio.CMCompareResult
import qualified Biobase.SElab.CM.Types as CM
import qualified Biobase.SElab.CM.ModelStructure as CM
import Biobase.SElab.CM.Import (cmFromFile)
import qualified Bio.HMMParser as HM
import System.Console.CmdArgs
import Data.Either
import qualified Data.Either.Unwrap as E
import System.Directory
import qualified Data.Text as T
import Paths_cmv (version)
import Data.Version (showVersion)
import Data.List (intercalate)
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Bio.HMMCompareResult as HC
import qualified Data.Vector as V

options :: Options
data Options = Options
  { cmcompareResultFile :: String,
    cmmodelsFile :: String,
    hmmmodelsFile :: String,
    outputDirectoryPath :: String
  } deriving (Show,Data,Typeable)

options = Options
  { cmcompareResultFile = def &= name "r" &= help "Path to CMCompare result file",
    cmmodelsFile = def &= name "c" &= help "Path to covariance model file",
    hmmmodelsFile = def &= name "h" &= help "Path to hmm model file"
  } &= summary ("CMCtoHMMC " ++ toolVersion) &= help "Florian Eggenhofer - 2013-2017" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  --Validate input
  cmmodelFileExists <- doesFileExist cmmodelsFile
  hmmodelFileExists <- doesFileExist hmmmodelsFile
  cmcFileExists <- doesFileExist cmcompareResultFile
  if cmmodelFileExists && hmmodelFileExists && cmcFileExists
     then do
       cms <- cmFromFile modelsFile
       inputhmms <- HM.readHMMER3 modelsFile
       let hmms = E.fromRight inputhmms
       cmcResultParsed <- getCmcompareResults cmcompareResultFile
       let comparisons = rights cmcResultParsed
       let cmVector = V.fromList cms
       let hmmVector = V.fromList hmms
       --let modelNumber = length cms
       let cmModelNames = map (T.unpack . CM._name) cms
       let hmmModelNames = map HM.name models
       currentWD <- getCurrentDirectory
       let dirPath = if null outputDirectoryPath then currentWD else outputDirectoryPath
       let modelFileNames = map (\m -> m ++ "." ++ outputFormat) currentModelNames
       setCurrentDirectory dirPath
       let hmmcs = map (cmctohmmv cmModelNames hmmModelNames cmVector hmmVector) comparisons
       print hmmcs
     else do
       Control.Monad.unless cmmodelFileExists $ putStrLn "CM Model file not found"
       Control.Monad.unless hmmmodelFileExists $ putStrLn "HMM Model file not found"
       Control.Monad.unless cmcFileExists $ putStrLn "Comparison file not found"

toolVersion :: String
toolVersion = showVersion version

cmctohmmv :: [String] -> [String] -> V.Vector CM.CM -> V.Vector HM.HMMER3 -> CmcompareResult -> HC.HMMCompareResult -- > HMMCompareResult
cmctohmmv cmModelNames hmmModelNames cmVector hmmVector comparison = hmmcompare
  where cm1Index = elemIndex model1Name comparisons
        cm2Index = elemIndex model2Name comparisons
        cm1 = cmVector V.! cm1Index
        cm2 = cmVector V.! cm2Index
        hmm1 = hmmVector V.! cm1Index
        hmm2 = hmmVector V.! cm2Index
        cm1nodeIndices = nub (model1matchednodes comparison)
        cm2nodeIndices = nub (model2matchednodes comparison)
        cm1nodes = V.fromList (M.elems (CM._fmNodes cm1))
        cm2nodes = V.fromList (M.elems (CM._fmNodes cm2))
        hmm1nodes = HM.nodes hmm1
        hmm2nodes = HM.nodes hmm2
        aln1colIndices = concatMap nodeToColumnIndex cm1nodes
        aln2colIndices = concatMap nodeToColumnIndex cm1nodes
        linkedhmm1nodes = V.filter (\node -> elem (fromJust (HM.nma node)) aln1colIndices) hmm1nodes
        linkedhmm2nodes = V.filter (\node -> elem (fromJust (HM.nma node)) aln1colIndices) hmm2nodes
        hmm1interval = V.map HM.nodeId linkedhmm1nodes
        hmm2interval = V.map HM.nodeId linkedhmm2nodes
        hmmcompare = HC.HMMCompareResult (model1Name comparison) (model2Name comparison) (linkscore1 comparison) (linkscore2 comparison) (linksequence comparison) hmm1interval hmm2interval

nodeToColumnIndices:: V.Vector CM.Node -> Int -> [Int]
nodeToColumnIndices nodes nodeIndex = colLabels
  where currentNode = (V.!) nodes (nodeIndex)
        colIndices = nub [CM._nodeColL currentNode,CM._nodeColR currentNode]

writeModelNameFile :: Bool -> String -> [String] -> IO ()
writeModelNameFile toggle outputDirectoryPath modelNames =
  if toggle
    then do
       let modelNamesString = intercalate "," modelNames
       writeFile (outputDirectoryPath ++ "modelNames") modelNamesString
    else return ()
