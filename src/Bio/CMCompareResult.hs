-- | Parse CMCompare output
--   parsing is done with parsec
--   For more information on CMCompare consult: <http://www.tbi.univie.ac.at/software/cmcompare/>

module Bio.CMCompareResult
    (
     CmcompareResult(..),
     getCmcompareResults,
     getModelsNames,
     getModelNames
    ) where

import Text.ParserCombinators.Parsec
import Data.List

-- | Datastructure for result strings of comparisons between covariance models by CMCompare
data CmcompareResult = CmcompareResult
  { model1Name :: String,
    model2Name :: String,
    linkscore1 :: Double,
    linkscore2 :: Double,
    linksequence  :: String,
    model1structure :: String,
    model2structure :: String,
    model1matchednodes :: [Int],
    model2matchednodes :: [Int]
  } deriving ()

instance Show CmcompareResult where
  show (CmcompareResult _model1Name _model2Name _linkscore1 _linkscore2 _linksequence _model1structure _model2structure _model1matchednodes _model2matchednodes) =  _model1Name ++ "   " ++ _model2Name ++ "     " ++ show _linkscore1 ++ "     " ++ show _linkscore2 ++ " " ++ _linksequence ++ " " ++ _model1structure ++ " " ++ _model2structure ++ " " ++ formatMatchedNodes _model1matchednodes ++ " " ++ formatMatchedNodes _model2matchednodes ++ "\n"
  
formatMatchedNodes :: [Int] -> String
formatMatchedNodes nodes = "[" ++ intercalate "," (map show nodes) ++ "]"

readDouble :: String -> Double
readDouble = read

readInt :: String -> Int
readInt = read

-- | Parse a CMcompare result string
parseCmcompareResult :: GenParser Char st CmcompareResult
parseCmcompareResult = do
    name1 <-  many1 (noneOf " ")
    _ <- many1 space
    name2 <-  many1 (noneOf " ")
    _ <- many1 space
    score1 <- many1 (noneOf " ")
    _ <- many1 space
    score2 <- many1 (noneOf " ")
    _ <- many1 space
    linkseq <- many1 (oneOf "AGTCUagtcu")
    _ <- many1 space
    structure1 <- many1 (oneOf "(,.)")
    _ <- many1 space
    structure2 <- many1 (oneOf "(,.)")
    _ <- many1 space
    _ <- char '['
    nodes1 <- many1 parseMatchedNodes
    _ <- char ']'
    _ <- many1 space
    _ <- char '['
    nodes2 <- many1 parseMatchedNodes
    _ <- char ']'
    return $ CmcompareResult name1 name2 (readDouble score1) (readDouble score2) linkseq structure1 structure2 nodes1 nodes2

-- | Parse indices of matched nodes between models as integers
parseMatchedNodes :: GenParser Char st Int
parseMatchedNodes = do
    nodeNumber <- many1 digit
    optional (char ',')
    return (readInt nodeNumber)

-- | Parser for CMCompare result strings
getCmcompareResults :: FilePath -> IO [Either ParseError CmcompareResult]
getCmcompareResults filePath = let
        fp = filePath
        doParseLine' = parse parseCmcompareResult "parseCMCompareResults"
    in do
        fileContent <- fmap lines $ readFile fp
        return $ map doParseLine' fileContent

getModelsNames :: [CmcompareResult] -> [String]
getModelsNames models = concatMap getModelNames models

getModelNames :: CmcompareResult -> [String]
getModelNames model = [model1Name model,model2Name model]
