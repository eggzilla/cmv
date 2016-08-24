-- | Parse HMM models
--   Currently contains a parser for HMMER3 models
--   For more information on HMMER consult: <http://hmmer.org>
module Bio.HMMParser (
                       parseHMMER3,
                       readHMMER3,                                   
                       module Bio.HMMData
                      ) where

import Bio.HMMData
import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import qualified Control.Exception.Base as CE

-- | parse HMMER3 model from input string
parseHMMER3 :: [Char] -> Either ParseError HMMER3
parseHMMER3 input = parse genParseHMMER3 "HMMER3" input

-- | parse HMMER3 from input filePath                      
readHMMER3 :: String -> IO (Either ParseError HMMER3)                  
readHMMER3 filePath = do 
  parsedFile <- parseFromFile genParseHMMER3 filePath
  CE.evaluate parsedFile

-- | Parse the input as HMMER3 datatype
genParseHMMER3 :: GenParser Char st HMMER3
genParseHMMER3 = do
  string "HMMER3/"
  _version <- many1 (noneOf "\n")
  newline
  string "NAME  "
  _name <- many1 alphaNum
  newline
  string "ACC   "
  _acc  <- many1 alphaNum
  newline
  string "DESC  "
  _desc <- many1 (noneOf "\n")
  newline
  string "LENG  "
  _leng <- parseIntegral
  newline
  string "MAXL  "
  _maxl <- parseIntegral
  newline
  string "ALPH  "
  _alpha <- many1 letter
  newline
  string "RF    "
  _rf <- parseSwitch
  newline
  string "MM    "
  _mm <- parseSwitch
  newline
  string "CONS  "
  _cons <- parseSwitch
  newline
  string "CS    "
  _cs <- parseSwitch
  newline
  string "MAP   "
  _map <- parseSwitch
  newline
  string "DATE  "
  _date <- many1 (noneOf "\n")
  newline
  string "COM   "
  _com <- many1 (noneOf "\n")
  newline
  string "NSEQ  "
  _nseq <- parseIntegral
  newline
  string "EFFN  "
  _effn <- parseFloat
  newline
  string "CKSUM "
  _cksum <- parseIntegral
  newline
  string "GA    "
  _ga <- parseFloat
  newline
  string "TC    "
  _tc <- parseFloat
  newline
  string "NC    "
  _nc <- parseFloat
  newline
  string "STATS LOCAL MSV       "
  _localmsv <- parseStat
  newline
  string "STATS LOCAL VITERBI   "
  _localviterbi <- parseStat
  string "STATS LOCAL FORWARD   "
  _localforward <- parseStat
  string "HMM"
  _hmm <- many1 parseAlphabetSymbol
  newline
  _compo <- parseCOMPOsite
  _nodes <- many1 parseNodes
  string "//"
  eof
  return $ HMMER3 _version _name _acc _desc _leng _maxl _alpha _rf _mm _cons _cs _map _date _com _nseq _effn _cksum _ga _tc _nc _localmsv _localviterbi _localforward _hmm _compo _nodes

parseSwitch :: GenParser Char st Bool
parseSwitch
parseStat :: GenParser Char st (Double,Double)
parseStat
parseAlphabetSymbol :: GenParser Char st Char
parseAlphabetSymbol 
parseCOMPOsite  :: GenParser Char st HMMER3node
parseCOMPOsite


-- | Parse HMMER3 node
parseHMMER3Node :: GenParser Char st HMMER3node
parseHMMER3Node = do
  _nodeNumber <- 
  _matchEmissions <- 
  _nma <- 
  _ncs <- 
  _nra <- 
  _nmv <- 
  _ncs <- 
  _insertEmissions <-
  _transitions <-
  return $ HMMER3 _nodeNumber _matchEmissions _nma _ncs _nra _nmv _ncs _insertEmissions _transitions
