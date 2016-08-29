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
import qualified Data.Vector as V

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
  _mm <- parseSwitchAttribute "MM"
  _cons <- parseSwitchAttribute "CONS"
  _cs <- parseSwitchAttribute "CS"
  _map <- parseSwitchAttribute "MAP"
  _date <- parseStringAttribute "DATE"
  _com <- parseStringAttribute "COM"
  _nseq <- parseIntAttribute "NSEQ"
  _effn <- parseFloatAttribute "EFFN"
  _cksum <- parseIntAttribute "CKSUM "
  _ga <- parseFloatAttribute "GA"
  _tc <- parseFloatAttribute "TC"
  _nc <- parseFloatAttribute "NC"
  _localmsv <- parseStatAttribute "STATS LOCAL MSV"
  _localviterbi <- parseStatAttribute "STATS LOCAL VITERBI"
  _localforward <- parseStatAttribute "STATS LOCAL FORWARD"
  string "HMM"
  _hmm <- many1 parseAlphabetSymbol
  newline
  many1 (noneOf "\n")
  newline
  _compo <- parseHMMER3Node
  _nodes <- many1 (parseHMMER3Node _alph)
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
  return _string

parseFloatAttribute :: String ->  GenParser Char st Double
parseFloatAttribute fieldName = do
  string fieldName
  many1 (oneOf " ")
  _double <- parseFloat
  newline
  return _double

parseIntAttribute :: String ->  GenParser Char st Int
parseIntAttribute fieldName = do
  string fieldName
  many1 (oneOf " ")
  _int <- parseIntegral
  newline
  return _int

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
parseHMMER3Node :: String -> GenParser Char st HMMER3Node
parseHMMER3Node alphabet = do
  many1 (oneOf " ")
  _nodeId <- many1 (noneOf " ")
  many1 (oneOf " ")
  _matchEmissions <- count (setEmissionNumber alphabet) (noneOf "\n")
  _nma <- many1 (noneOf "\n")
  _ncs <- many1 (noneOf "\n")
  _nra <- many1 (noneOf "\n")
  _nmv <- many1 (noneOf "\n")
  _ncs <- many1 (noneOf "\n")
  newline
  _insertEmissions <- many1 (noneOf "\n")
  newline
  _transitions <- many1 (noneOf "\n")
  return $ HMMER3 _nodeId _matchEmissions _nma _ncs _nra _nmv _ncs _insertEmissions _transitions

setEmissionNumber :: String -> Int
setEmissionNumber alphabet
  | alphabet == "DNA" = 4
  | alphabet == "RNA" = 4 
  | alphabet == "amino" = 20
  | otherwise = 20

parseDoubleParameter :: GenParser Char st Double
parseDoubleParameter = do
  many1 (oneOf " ")
  _float <- parseFloat
  return _float
