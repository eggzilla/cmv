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
  _version <- many1 (noneOf " ")
  _name <-
  _acc  <-
  _desc <-
  _leng <-
  _maxl <-
  _alpha <-
  _rf <-
  _mm <-
  _cons <-
  _cs <-
  _map <-
  _date <-
  _com <-
  _nseq <-
  _effn <-
  _cksum <-
  _ga <-
  _tc <-
  _nc <-
  _localmsv <-
  _localviterbi <-
  _localforward <-
  _hmm <-
  _compo <-
  _nodes <-
  return $ HMMER3 _version 

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
