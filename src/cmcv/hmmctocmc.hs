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
import Data.List

options :: Options
data Options = Options
  { hmmcompareResultFile :: String,
    cmmodelsFile :: String,
    hmmmodelsFile :: String
  } deriving (Show,Data,Typeable)

options = Options
  { hmmcompareResultFile = def &= name "r" &= help "Path to HMMCompare result file",
    cmmodelsFile = def &= name "c" &= help "Path to covariance model file",
    hmmmodelsFile = def &= name "h" &= help "Path to hmm model file"
  } &= summary ("HMMCtoCMC " ++ toolVersion) &= help "Florian Eggenhofer - 2013-2017" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  --Validate input
  cmmodelFileExists <- doesFileExist cmmodelsFile
  hmmmodelFileExists <- doesFileExist hmmmodelsFile
  cmcFileExists <- doesFileExist hmmcompareResultFile
  if cmmodelFileExists && hmmmodelFileExists && cmcFileExists
     then do
       cms <- cmFromFile cmmodelsFile
       inputhmms <- HM.readHMMER3 hmmmodelsFile
       let hmms = E.fromRight inputhmms
       hmmcResultParsed <- HC.getHMMCompareResults hmmcompareResultFile
       let comparisons = rights hmmcResultParsed
       let comparisonse = lefts hmmcResultParsed
       print comparisonse
       let cmVector = V.fromList cms
       let hmmVector = V.fromList hmms
       --let modelNumber = length cms
       let cmModelNames = map (T.unpack . CM._name) cms
       let hmmModelNames = map HM.name hmms
       --currentWD <- getCurrentDirectory
       --let dirPath = if null outputDirectoryPath then currentWD else outputDirectoryPath
       --setCurrentDirectory dirPath
       let cmcs = map (hmmctocmc cmModelNames hmmModelNames cmVector hmmVector) comparisons
       mapM_ (putStr . show) cmcs
     else do
       Control.Monad.unless cmmodelFileExists $ putStrLn "CM Model file not found"
       Control.Monad.unless hmmmodelFileExists $ putStrLn "HMM Model file not found"
       Control.Monad.unless cmcFileExists $ putStrLn "Comparison file not found"

toolVersion :: String
toolVersion = showVersion version

hmmctocmc :: [String] -> [String] -> V.Vector CM.CM -> V.Vector HM.HMMER3 -> HC.HMMCompareResult -> CmcompareResult
hmmctocmc cmModelNames hmmModelNames cmVector hmmVector comparison = cmcompare
  where cm1Index = fromJust (elemIndex (HC.model1Name comparison) cmModelNames)
        cm2Index = fromJust (elemIndex (HC.model2Name comparison) cmModelNames)
        inputCM1 = cmVector V.! cm1Index
        cm1 = E.fromLeft (CM._cm inputCM1)     
        inputCM2 = cmVector V.! cm2Index
        cm2 = E.fromLeft (CM._cm inputCM2)
        hmm1Index = fromJust (elemIndex (HC.model1Name comparison) hmmModelNames)
        hmm2Index = fromJust (elemIndex (HC.model2Name comparison) hmmModelNames)
        hmm1 = hmmVector V.! hmm1Index
        hmm2 = hmmVector V.! hmm2Index
        hmm1nodeIndices = nub (HC.model1matchednodes comparison)
        hmm2nodeIndices = nub (HC.model2matchednodes comparison)
        cm1nodes = (M.elems (CM._fmNodes cm1))
        cm2nodes = (M.elems (CM._fmNodes cm2))
        hmm1nodes = HM.nodes hmm1
        hmm2nodes = HM.nodes hmm2
        --filter cmnodes
        aln1colIndices = map (\i -> fromJust $ HM.nma (hmm1nodes V.! (i-1))) hmm1nodeIndices
        aln2colIndices = map (\i -> fromJust $ HM.nma (hmm2nodes V.! (i-1))) hmm2nodeIndices
        -- aln1colIndices = concatMap nodeToColumnIndices cm1nodes
        -- aln2colIndices = concatMap nodeToColumnIndices cm2nodes
        -- linkedcm1nodes = V.filter (\node -> elem (fromJust (HM.nma node)) aln1colIndices) cm1nodes
        -- linkedcm2nodes = V.filter (\node -> elem (fromJust (HM.nma node)) aln2colIndices) cm2nodes
        -- cm1interval = V.map HM.nodeId linkedcm1nodes
        -- cm2interval = V.map HM.nodeId linkedcm2nodes
        cm1interval = findIndices (\n -> or $ map (\i -> elem i aln1colIndices) (nodeToColumnIndices n)) cm1nodes
        cm2interval = findIndices (\n -> or $ map (\i -> elem i aln2colIndices) (nodeToColumnIndices n)) cm2nodes
        cmcompare = CmcompareResult (HC.model1Name comparison) (HC.model2Name comparison) (HC.linkscore1 comparison) (HC.linkscore2 comparison) (HC.linksequence comparison) (replicate (length cm1interval) '.') (replicate (length cm2interval) '.') cm1interval cm2interval

nodeToColumnIndices:: CM.Node -> [Int]
nodeToColumnIndices currentNode = nub [CM._nodeColL currentNode,CM._nodeColR currentNode]
--  where currentNode = (V.!) nodes (nodeIndex)
--        colIndices = nub [CM._nodeColL currentNode,CM._nodeColR currentNode]

writeModelNameFile :: Bool -> String -> [String] -> IO ()
writeModelNameFile toggle outputDirectoryPath modelNames =
  if toggle
    then do
       let modelNamesString = intercalate "," modelNames
       writeFile (outputDirectoryPath ++ "modelNames") modelNamesString
    else return ()
