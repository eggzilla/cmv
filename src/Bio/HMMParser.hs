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
  _name <- many1 (noneOf "\n")
  newline
  string "ACC   "
  _acc  <- many1 (noneOf "\n")
  newline
  string "DESC  "
  _desc <- many1 (noneOf "\n")
  newline
  string "LENG  "
  _leng <- many1 (noneOf "\n")
  newline
  string "MAXL  "
  _maxl <- many1 (noneOf "\n")
  newline
  string "ALPH  "
  _alpha <- many1 (noneOf "\n")
  newline
  string "RF    "
  _rf <- many1 (noneOf "\n")
  newline
  string "MM    "
  _mm <- many1 (noneOf "\n")
  newline
  string "CONS  "
  _cons <- many1 (noneOf "\n")
  newline
  string "CS    "
  _cs <- many1 (noneOf "\n")
  newline
  string "MAP   "
  _map <- many1 (noneOf "\n")
  newline
  string "DATE  "
  _date <- many1 (noneOf "\n")
  newline
  string "COM   "
  _com <- many1 (noneOf "\n")
  newline
  string "NSEQ  "
  _nseq <- many1 (noneOf "\n")
  newline
  string "EFFN  "
  _effn <- many1 (noneOf "\n")
  newline
  string "CKSUM "
  _cksum <- many1 (noneOf "\n")
  newline
  string "GA    "
  _ga <- many1 (noneOf "\n")
  newline
  string "TC    "
  _tc <- many1 (noneOf "\n")
  newline
  string "NC    "
  _nc <- many1 (noneOf "\n")
  newline
  string "STATS LOCAL MSV       "
  _localmsv <- many1 (noneOf "\n")
  newline
  string "STATS LOCAL VITERBI   "
  _localviterbi <-
  string "STATS LOCAL FORWARD   "
  _localforward <-
  string "HMM"
  _hmm <- many1 parseAlphabetSymbol
  newline
  _compo <- parseCOMPOsite
  _nodes <- many1 parseNodes
  string "//"
  eof
  return $ HMMER3 _version 

parseAlphabetSymbol

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
