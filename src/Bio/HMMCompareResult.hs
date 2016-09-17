-- | Parse HMMCompare output

module Bio.HMMCompareResult
    (
     HMMcompareResult,
     model1Name,
     model2Name,
     linkscore1, 
     linkscore2,
     linksequence,
     model1matchednodes,
     model2matchednodes
    ) where

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad

-- | Datastructure for result strings of comparisons between covariance models by HMMCompare
data HMMcompareResult = HMMcompareResult           
  { model1Name :: String,
    model2Name :: String,
    linkscore1 :: Double,
    linkscore2 :: Double,
    linksequence  :: String,
    model1matchednodes :: MatchedNodes,
    model2matchednodes :: MatchedNodes
  } deriving (Show)

-- | Type alias for matched nodes
type MatchedNodes = [Int]

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

-- | Parse a floating point number.
parseFloat :: GenParser Char st Double
parseFloat = do sign <- option 1 (do s <- oneOf "+-"
                                     return $ if s == '-' then-1.0 else 1.0)
                x  <- float $ makeTokenParser emptyDef
                return $ sign * x

-- | Parse a HMMcompare result string
parseHMMcompareResult :: GenParser Char st HMMcompareResult
parseHMMcompareResult = do
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
    _ <- char '['
    nodes1 <- many1 parseMatchedNodes
    _ <- char ']'
    _ <- many1 space
    _ <- char '['
    nodes2 <- many1 parseMatchedNodes
    _ <- char ']'
    return $ HMMcompareResult name1 name2 (readDouble score1) (readDouble score2) linkseq structure1 structure2 nodes1 nodes2

-- | Parse indices of matched nodes between models as integers
parseMatchedNodes :: GenParser Char st Int 
parseMatchedNodes = do
    nodeNumber <- many1 digit
    optional (char ',')
    return $ (readInt nodeNumber)

-- | Parser for HMMCompare result strings
getHMMcompareResults :: FilePath -> IO [Either ParseError HMMcompareResult]    
getHMMcompareResults filePath = let
        fp = filePath
        doParseLine' = parse parseHMMcompareResult "parseHMMCompareResults"
        doParseLine l = case (doParseLine' l) of
            Right x -> x
            Left _  -> error "Failed to parse line"
    in do
        fileContent <- liftM lines $ readFile fp
        return $ map doParseLine' fileContent

getModelsNames :: [HMMCompareResult] -> [String]
getModelsNames models = concat (map getModelNames models)

getModelNames :: HMMCompareResult -> [String]
getModelNames model = [model1Name model,model2Name model]
