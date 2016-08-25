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
  _version <- parseStringAttribute "HMMER3/"
  _name <- parseStringAttribute "NAME"
  _desc <- parseStringAttribute "DESC"
  _leng <- parseIntAttribute "LENG"
  --string "LENG  "
  --_leng <- parseIntegral
  --newline
  _maxl <- parseIntAttribute "MAXL"
  --string "MAXL  "
  --_maxl <- parseIntegral
  --newline
  _alph <- parseStringAttribute "ALPH"
  --string "ALPH  "
  --_alpha <- many1 letter
  --newline
  _rf <- parseSwitchAttribute "RF"
  _nc <- parseSwitchAttribute "MM"
  _nc <- parseSwitchAttribute "CONS"
  _nc <- parseSwitchAttribute "CS"
  _nc <- parseSwitchAttribute "MAP"
  _nc <- parseStringAttribute "DATE"
  _nc <- parseStringAttribute "COM"
  _nc <- parseIntAttribute "NSEQ"
  _nc <- parseFloatAttribute "EFFN"
  _nc <- parseIntAttribute "CKSUM "
  _nc <- parseFloatAttribute "GA"
  _nc <- parseFloatAttribute "TC"
  _nc <- parseFloatAttribute "NC"
  _localmsv <- parseStatAttribute "STATS LOCAL MSV"
  _localviterbi <- parseStatAttribute "STATS LOCAL VITERBI"
  _localforward <- parseStatAttribute "STATS LOCAL FORWARD"
  string "HMM"
  _hmm <- many1 parseAlphabetSymbol
  newline
  _compo <- parseHMMER3Node
  _nodes <- many1 parseHMMER3Node
  string "//"
  eof
  return $ HMMER3 _version _name _acc _desc _leng _maxl _alpha _rf _mm _cons _cs _map _date _com _nseq _effn _cksum _ga _tc _nc _localmsv _localviterbi _localforward (V.fromList _hmm) _compo _nodes

parseSwitchAttribute :: String -> GenParser Char st Bool
parseSwitchAttribute fieldName = do
  string fieldName
  many1 (oneOf " ")
  _switch <- try (string "Yes") <|> try (string "No")
  return switchToBool _switch

switchToBool :: String -> Bool
switchToBool switch
  | switch == "yes" = True
  | otherwise = False

parseStringAttribute :: String ->  GenParser Char st String
parseStringAttribute fieldName = do
  string fieldName
  many1 (oneOf " ")
  _string <- many1 (noneOf "\n")
  newline
  return string

parseFloatAttribute :: String ->  GenParser Char st Double
parseFloatAttribute fieldName = do
  string fieldName
  many1 (oneOf " ")
  _double <- many1 (noneOf "\n")
  newline
  return double

parseIntAttribute :: String ->  GenParser Char st Int
parseIntAttribute fieldName = do
  string fieldName
  many1 (oneOf " ")
  _int <- parseIntegral
  newline
  return int

parseStatAttribute :: String -> GenParser Char st (Double,Double)
parseStatAttribute fieldName = do
  string fieldName
  many1 (oneOf " ")
  _stat1 <- parseFloat
  many1 (oneOf " ")
  _stat2 <- parseFloat
  return (_stat1,_stat2)
  
parseAlphabetSymbol :: GenParser Char st Char
parseAlphabetSymbol = do
  many1 (oneOf " ")
  _symbol <- upper
  return _symbol
  
-- | Parse HMMER3 node
parseHMMER3Node :: GenParser Char st HMMER3Node
parseHMMER3Node = do
  _nodeId <- many1 (noneOf "\n")
  _matchEmissions <- many1 (noneOf "\n")
  _nma <- many1 (noneOf "\n")
  _ncs <- many1 (noneOf "\n")
  _nra <- many1 (noneOf "\n")
  _nmv <- many1 (noneOf "\n")
  _ncs <- many1 (noneOf "\n")
  _insertEmissions <- many1 (noneOf "\n")
  _transitions <- many1 (noneOf "\n")
  return $ HMMER3 _nodeId _matchEmissions _nma _ncs _nra _nmv _ncs _insertEmissions _transitions
