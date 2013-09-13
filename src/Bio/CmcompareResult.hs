-- | Parse CMCompare output
--   parsing is done with parsec
--   For more information on CMCompare consult: <>

module CmcompareResult
    (
     getCmcompareResults
    ) where

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad
    
data CmcompareResult = CmcompareResult           
  { model1Name :: String,
    model2Name :: String,
    linkscore1 :: Double,
    linkscore2 :: Double,
    linksequence  :: String,
    model1structure :: String,
    model2structure :: String,
    model1matchednodes :: MatchedNodes,
    model2matchednodes :: MatchedNodes
  } deriving (Show)

type MatchedNodes = [Int]

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

parseFloat :: GenParser Char st Double
parseFloat = do sign <- option 1 (do s <- oneOf "+-"
                                     return $ if s == '-' then-1.0 else 1.0)
                x  <- float $ makeTokenParser emptyDef
                return $ sign * x
             
parseCmcompareResult = do
    name1 <-  many1 (noneOf " ")
    many1 space
    name2 <-  many1 (noneOf " ")
    many1 space
    score1 <- many1 (noneOf " ")
    many1 space
    score2 <- many1 (noneOf " ")
    many1 space
    linkseq <- many1 (oneOf "AGTCUagtcu")
    many1 space
    structure1 <- many1 (oneOf "(,.)")
    many1 space
    structure2 <- many1 (oneOf "(,.)")
    many1 space
    char '['
    nodes1 <- many1 parseMatchedNodes
    char ']'
    many1 space
    char '['
    nodes2 <- many1 parseMatchedNodes
    char ']'
    return $ CmcompareResult name1 name2 (readDouble score1) (readDouble score2) linkseq structure1 structure2 nodes1 nodes2

parseMatchedNodes = do
    nodeNumber <- many1 digit
    optional (char ',')
    return $ (readInt nodeNumber)

getCmcompareResults filePath = let
        fp = filePath
        doParseLine' = parse parseCmcompareResult "parseCMCompareResults"
        doParseLine l = case (doParseLine' l) of
            Right x -> x
            Left _ -> error "Failed to parse line"
    in do
        fileContent <- liftM lines $ readFile fp
        return $ map doParseLine' fileContent
