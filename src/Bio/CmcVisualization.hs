{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

-- | Visualize detailed comparsions of Infernal Covariance Models
--   Datastructures and parsing of covariance models is provided by Biobase
--   Visualization is accomplished with diagrams-svg
--   For more information on Infernal consult <http://meme.nbcr.net/meme/>


module Main where

import qualified Diagrams.Prelude as Diag
import Diagrams.Backend.SVG
import Control.Monad
    
import Biobase.Primary
import qualified Biobase.SElab.CM as CM
import Biobase.SElab.CM.Import
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
    
import System.Console.CmdArgs
import Text.Printf

data Options = Options            
  { cmcompareResultFile :: CmcompareResultFile,
    modelsFile :: ModelsFile
  } deriving (Show,Data,Typeable)

type CmcompareResultFile = String
type ModelsFile = String
             
data CmcompareResult = CmcompareResult           
  { model1Name :: String,
    model2Name :: String,
    linkscore1 :: Double,
    linkscore2 :: Double,
    linksequence  :: String,
    model1structure :: String,
    model2structure :: String,
    model1matchednodes :: String,
    model2matchednodes :: String
  } deriving (Show,Data,Typeable)

             
options = Options
  { cmcompareResultFile = def &= name "r" &= help "Path to CMCompare result file",
    modelsFile = def &= name "m" &= help "Path to covariance model file"
  } &= summary "CMCV devel version" &= help "Florian Eggenhofer - 2013" &= verbosity

readDouble :: String -> Double
readDouble = read              

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
    nodes1 <- many1 (noneOf " ")
    many1 space
    nodes2 <- many1 (noneOf " ")
    return $ CmcompareResult name1 name2 (readDouble score1) (readDouble score2) linkseq structure1 structure2 nodes1 nodes2

getCmcompareResults filePath = let
        fp = filePath
        doParseLine' = parse parseCmcompareResult "parseCMCompareResults"
        doParseLine l = case (doParseLine' l) of
            Right x -> x
            Left _ -> error "Failed to parse line"
    in do
        fileContent <- liftM lines $ readFile fp
        return $ map doParseLine' fileContent
           
main = do
  Options{..} <- cmdArgs options
  let a = modelsFile
  let b = cmcompareResultFile
  models <- fromFile a
  print a
  print b
  cmResultParsed <- getCmcompareResults b
  --print models
  print cmResultParsed  
