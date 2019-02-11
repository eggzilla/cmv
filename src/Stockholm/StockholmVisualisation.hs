{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Visualize Stockholm models
--   Visualization is accomplished with diagrams-svg

module Main where

import System.Console.CmdArgs
import System.Directory
import Data.Either.Unwrap
import qualified Bio.StockholmParser as SP
import qualified Data.Text as T
import Paths_cmv (version)
import Data.Version (showVersion)
--import Data.List (intercalate)
import Control.Monad
--Data.Maybe
import Bio.StockholmDraw
import Bio.CMDraw
import qualified Data.Char as C
--import qualified Data.Vector as V

options :: Options
data Options = Options
  { alignmentFile :: String,
    alignmentEntries :: Int,
    maxWidth :: Double,
    scalingFactor :: Double,
    outputFormat :: String,
    outputDirectoryPath :: String,
    secondaryStructureVisTool :: String
  } deriving (Show,Data,Typeable)

options = Options
  { alignmentFile = "" &= name "s" &= help "Path to stockholm alignment file",
    alignmentEntries = (50 :: Int) &= name "n" &= help "Set cutoff for included stockholm alignment entries (Default: 50)",
    maxWidth = (200 :: Double) &= name "w" &= help "Set maximal width of result figure (Default: 100)",
    scalingFactor = (2.0 :: Double) &= name "c" &= help "Set uniform scaling factor for image size (Default: 2)",
    outputFormat = "pdf" &= name "f" &= help "Output image format: pdf, svg, png, ps (Default: pdf)",
    outputDirectoryPath = "" &= name "p" &= help "Output directory path (Default: none)",
    secondaryStructureVisTool = "" &= name "x" &= help "Select tool for secondary structure visualisation: forna, r2r (Default: none)"
  } &= summary ("StockholmV " ++ toolVersion) &= help "Florian Eggenhofer - 2019-" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  alnFileExists <- doesFileExist alignmentFile
  if alnFileExists
    then do
      alnInput <- SP.readExistingStockholm alignmentFile
      Control.Monad.when (isLeft alnInput) $ print (fromLeft alnInput)
      let alns = fromRight alnInput
      currentWD <- getCurrentDirectory
      let dirPath = if null outputDirectoryPath then currentWD else outputDirectoryPath
      let currentAlnNames = map show [1..(length alns)]
      let alignmentFileNames = map (\m -> m ++ ".aln" ++ "." ++ outputFormat) currentAlnNames
      setCurrentDirectory dirPath
      let alignmentVis = map (drawStockholm alignmentEntries) alns
      mapM_ (\(alnPath,stockholm) -> printCM alnPath svgsize stockholm) (zip alignmentFileNames alignmentVis)
      let structureFilePath = dirPath ++ "/"
      let structureVisInputs = zip currentAlnNames alns
      let structureVisOutputs = perAlignmentSecondaryStructureVisualisation secondaryStructureVisTool maxWidth structureFilePath structureVisInputs
      mapM_ (\(structureFileName,structureVis) -> writeFile structureFileName structureVis) structureVisOutputs
      setCurrentDirectory currentWD
    else
      putStrLn "Input alignment file not found"

toolVersion :: String
toolVersion = showVersion version

-- | Extracts consensus secondary structure from alignment and annotates cmcompare nodes for each model-model combination separately
perAlignmentSecondaryStructureVisualisation :: String -> Double -> String -> [(String,SP.StockholmAlignment)] -> [(String,String)]
perAlignmentSecondaryStructureVisualisation selectedTool _ structureFilePath alns
  | selectedTool == "forna" = fornaVis
  | selectedTool == "r2r" = r2rVis
  | selectedTool == "fornaLink" = fornaLink
  | selectedTool == "r2rfornaLink" = fornaLink ++ r2rVis
  | selectedTool == "all" = fornaLink ++ r2rVis ++ fornaVis
  | otherwise = []
  where fornaVis = map (buildFornaperAlignment structureFilePath) alns
        fornaLink = concatMap (buildFornaLinks structureFilePath)  alns
        r2rVis = concatMap (buildR2RperAlignment structureFilePath) alns

buildFornaperAlignment :: String -> (String,SP.StockholmAlignment) -> (String, String)
buildFornaperAlignment structureFilePath (alnName,aln) = fornaInput
  where fornaString = ">" ++ alnName ++ "\n" ++ gapfreeConsensusSequence ++ "\n" ++ gapFreeConsensusStructure
        fornaFilePath = structureFilePath ++ alnName ++ ".forna"
        fornaInput = (fornaFilePath,fornaString)
        allColumnAnnotations = SP.columnAnnotations aln
        consensusSequenceList = map SP.annotation (filter (\annotEntry -> SP.tag annotEntry == T.pack "RF") allColumnAnnotations)
        firstSeq = T.unpack (SP.entrySequence (head (SP.sequenceEntries aln)))
        consensusSequence = if null consensusSequenceList then firstSeq else T.unpack (head consensusSequenceList)
        gapfreeConsensusSequence = map C.toUpper (filter (not . isGap) consensusSequence)
        consensusStructureList = map (convertWUSStoDotBracket . SP.annotation) (filter (\annotEntry -> SP.tag annotEntry == T.pack "SS_cons") allColumnAnnotations)
        consensusStructure = if null consensusStructureList then "" else T.unpack (head consensusStructureList)
        indexedGapFreeConsensusStructure = extractGapfreeIndexedStructure consensusSequence consensusStructure
        --consensusStructureColIndices = map ((+1) . fst) indexedGapFreeConsensusStructure
        gapFreeConsensusStructure = map snd indexedGapFreeConsensusStructure
        --columnComparisonLabels = V.map (\(mname,mcolor,comparisonNodePerModelLabels) -> (mname,mcolor,getComparisonPerColumnLabels comparisonNodePerModelLabels nodes)) comparisonNodeLabelsPerModels
        --filter for labels that are part of consensus secondary structure by index
        --consensusStructureColumnComparisonLabels = V.map (\(mname,mcolor,colLabels) -> (mname,mcolor,V.filter (\(i,_) -> elem i consensusStructureColIndices) colLabels)) columnComparisonLabels
        --colorSchemes = V.toList (V.map (makeColorScheme modelName structureFilePath) consensusStructureColumnComparisonLabels)

buildFornaLinks :: String -> (String,SP.StockholmAlignment) -> [(String, String)]
buildFornaLinks structureFilePath (alnName,aln) = singleFornaLink
  where fornaURLPrefix = "http://rna.tbi.univie.ac.at/forna/forna.html?id=fasta&file=%3Eheader\\n" ++ gapfreeConsensusSequence ++ "\\n" ++ gapFreeConsensusStructure
        singleFornaLink = [(fornaFilePath,fornaURLPrefix)]
        fornaFilePath = structureFilePath ++ alnName ++ ".fornalink"
        allColumnAnnotations = SP.columnAnnotations aln
        consensusSequenceList = map SP.annotation (filter (\annotEntry -> SP.tag annotEntry == T.pack "RF") allColumnAnnotations)
        firstSeq = T.unpack (SP.entrySequence (head (SP.sequenceEntries aln)))
        consensusSequence = if null consensusSequenceList then firstSeq else T.unpack (head consensusSequenceList)
        gapfreeConsensusSequence = map C.toUpper (filter (not . isGap) consensusSequence)
        consensusStructureList = map (convertWUSStoDotBracket . SP.annotation) (filter (\annotEntry -> SP.tag annotEntry == T.pack "SS_cons") allColumnAnnotations)
        consensusStructure = if null consensusStructureList then "" else T.unpack (head consensusStructureList)
        indexedGapFreeConsensusStructure = extractGapfreeIndexedStructure consensusSequence consensusStructure
        --consensusStructureColIndices = map ((+1) . fst) indexedGapFreeConsensusStructure
        gapFreeConsensusStructure = map snd indexedGapFreeConsensusStructure



buildR2RperAlignment :: String -> (String, SP.StockholmAlignment) -> [(String,String)]
buildR2RperAlignment structureFilePath (alnName,aln) = singler2rInput
  where r2rInputPrefix = sHeader ++ sConsensusStructure ++ sConsensusSequence ++ sConsensusSequenceColor ++ sCovarianceAnnotation
        allColumnAnnotations = SP.columnAnnotations aln
        consensusSequenceList = map SP.annotation (filter (\annotEntry -> SP.tag annotEntry == T.pack "RF") allColumnAnnotations)
        firstSeq = T.unpack (SP.entrySequence (head (SP.sequenceEntries aln)))
        consensusSequence = if null consensusSequenceList then firstSeq else T.unpack (head consensusSequenceList)
        gapFreeConsensusSequence = map C.toUpper (filter (not . isGap) consensusSequence)
        consensusStructureList = map (convertWUSStoDotBracket . SP.annotation) (filter (\annotEntry -> SP.tag annotEntry == T.pack "SS_cons") allColumnAnnotations)
        consensusStructure = if null consensusStructureList then "" else T.unpack (head consensusStructureList)
        indexedGapFreeConsensusStructure = extractGapfreeIndexedStructure consensusSequence consensusStructure
        --consensusStructureColIndices = map ((+1) . fst) indexedGapFreeConsensusStructure
        gapFreeConsensusStructure = map snd indexedGapFreeConsensusStructure
        sHeader =  "# STOCKHOLM 1.0\n"
        sConsensusStructure =     "#=GC SS_cons          " ++ gapFreeConsensusStructure ++ "\n"
        sConsensusSequence =      "#=GC cons             " ++ gapFreeConsensusSequence ++ "\n" -- ++ show consensusStructureColIndices ++ "\n" ++ show comparisonNodeLabels ++ "\n"
        sConsensusSequenceColor = "#=GC conss            " ++ replicate (length gapFreeConsensusSequence) '2' ++ "\n"
        sCovarianceAnnotation =   "#=GC cov_SS_cons      " ++ replicate (length gapFreeConsensusSequence) '.' ++ "\n"
        singleFilePath = structureFilePath ++ alnName ++ ".r2r"
        singler2rInput = [(singleFilePath,r2rInputPrefix)]
