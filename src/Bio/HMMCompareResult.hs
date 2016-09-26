-- | Parse HMMCompare output

module Bio.HMMCompareResult
    (
     HMMCompareResult,
     parseHMMCompareResult,
     readHMMCompareResult,
     model1Name,
     model2Name,
     linkscore1, 
     linkscore2,
     linksequence,
     model1matchednodes,
     model2matchednodes,
     getHMMCompareResults,
     getModelsNames,
     getModelNames
    ) where

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad

-- | Datastructure for result strings of comparisons between covariance models by HMMCompare
data HMMCompareResult = HMMCompareResult           
  { model1Name :: String,
    model2Name :: String,
    linkscore1 :: Double,
    linkscore2 :: Double,
    linksequence  :: String,
    model1matchednodes :: [Int],
    model2matchednodes :: [Int]
  } deriving (Show)

-- | parse HMMCompareResult model from input string
parseHMMCompareResult :: [Char] -> Either ParseError [HMMCompareResult]
parseHMMCompareResult input = parse genParseHMMCompareResults "HMMCompareResult" input

-- | parse HMMCompareResult from input filePath                      
readHMMCompareResult :: String -> IO (Either ParseError [HMMCompareResult])                  
readHMMCompareResult filePath = do 
  parsedFile <- parseFromFile genParseHMMCompareResults filePath
  return parsedFile

-- | Parse the input as HMMCompareResult datatype
genParseHMMCompareResults :: GenParser Char st [HMMCompareResult]
genParseHMMCompareResults = do
  hmmcs  <- many1 (try genParseHMMCompareResult)
  eof
  return hmmcs

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

-- | Parse a HMMCompare result string
genParseHMMCompareResult :: GenParser Char st HMMCompareResult
genParseHMMCompareResult = do
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
    newline
    return $ HMMCompareResult name1 name2 (readDouble score1) (readDouble score2) linkseq nodes1 nodes2

-- | Parse indices of matched nodes between models as integers
parseMatchedNodes :: GenParser Char st Int 
parseMatchedNodes = do
    nodeNumber <- many1 digit
    optional (char ',')
    return $ (readInt nodeNumber)

-- | Parser for HMMCompare result strings
getHMMCompareResults :: FilePath -> IO [Either ParseError HMMCompareResult]    
getHMMCompareResults filePath = let
        fp = filePath
        doParseLine' = parse genParseHMMCompareResult "genParseHMMCompareResults"
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
