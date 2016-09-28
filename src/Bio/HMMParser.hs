-- | Parse HMM models
--   Currently contains a parser for HMMER3 models
--   For more information on HMMER consult: <http://hmmer.org>
module Bio.HMMParser (
                       parseHMMER3,
                       readHMMER3,
                       alphabetToSymbols,
                       module Bio.HMMData
                      ) where

import Bio.HMMData
import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import qualified Control.Exception.Base as CE
import qualified Data.Vector as V

-- | parse HMMER3 model from input string
parseHMMER3 :: [Char] -> Either ParseError [HMMER3]
parseHMMER3 input = parse genParseHMMER3s "HMMER3" input

-- | parse HMMER3 from input filePath                      
readHMMER3 :: String -> IO (Either ParseError [HMMER3])                  
readHMMER3 filePath = do 
  parsedFile <- parseFromFile genParseHMMER3s filePath
  CE.evaluate parsedFile

-- | Parse the input as HMMER3 datatype
genParseHMMER3s :: GenParser Char st [HMMER3]
genParseHMMER3s = do
  hmmers  <- many1 genParseHMMER3
  eof
  return hmmers

-- | Parse the input as HMMER3 datatype
genParseHMMER3 :: GenParser Char st HMMER3
genParseHMMER3 = do
  string "HMMER3/"
  _version <- many1 (noneOf "\n")
  newline
  _name <- parseStringAttribute "NAME"
  _acc <-  try (parseStringAttribute "ACC")
  _desc <- optionMaybe (try (parseStringAttribute "DESC"))         
  _leng <- parseIntAttribute "LENG"
  _maxl <- optionMaybe (try (parseIntAttribute "MAXL"))
  _alph <- parseStringAttribute "ALPH"
  _rf <- parseSwitchAttribute "RF"
  _mm <- parseSwitchAttribute "MM"
  _cons <- parseSwitchAttribute "CONS"
  _cs <- parseSwitchAttribute "CS"
  _map <- parseSwitchAttribute "MAP"
  _date <- parseStringAttribute "DATE"
  _com <- many (try (parseStringAttribute "COM"))
  _nseq <- parseIntAttribute "NSEQ"
  _effn <- parseFloatAttribute "EFFN"
  _cksum <- parseIntAttribute "CKSUM"
  _ga <- optionMaybe (try (parseStatAttribute "GA")) 
  _tc <- optionMaybe (try (parseStatAttribute "TC"))
  _nc <- optionMaybe (try (parseStatAttribute "NC"))
  _bm <- optionMaybe (try (parseStringAttribute "BM"))
  _sm <- optionMaybe (try (parseStringAttribute "SM"))
  _localmsv <- parseStatAttribute "STATS LOCAL MSV"
  _localviterbi <- parseStatAttribute "STATS LOCAL VITERBI"
  _localforward <- parseStatAttribute "STATS LOCAL FORWARD"
  string "HMM"
  _hmm <- many1 (try parseAlphabetSymbol)
  many1 (oneOf " ")
  newline
  many1 (noneOf "\n")
  newline
  (_compo,_begin) <- parseHMMER3Composite _alph
  _nodes <- many1 (parseHMMER3Node _alph)
  string "//"
  newline
  return $ HMMER3 _version _name _acc _desc _leng _maxl _alph _rf _mm _cons _cs _map _date (if null _com then Nothing else Just (concat _com)) _nseq _effn _cksum _ga _tc _nc _bm _sm _localmsv _localviterbi _localforward _hmm _compo _begin (V.fromList _nodes)

parseSwitchAttribute :: String -> GenParser Char st Bool
parseSwitchAttribute fieldName = do
  string fieldName
  many1 (oneOf " ")
  _switch <- try (string "yes") <|> try (string "no")
  newline
  return (switchToBool _switch)

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
  --many1 (oneOf " ")
  _stat2 <- parseFloat
  optional (string ";")
  newline
  return (_stat1,_stat2)
  
-- | Parse HMMER3 composite
parseHMMER3Composite :: String -> GenParser Char st (HMMER3Composite,HMMER3Node)
parseHMMER3Composite alphabet = do
  many1 (oneOf " ")
  _nodeId <- many1 (noneOf " ")
  _matchEmissions <- count (setEmissionNumber alphabet) (try parseDoubleParameter)  
  newline
  _insertEmissions <- many1 parseDoubleParameter
  newline
  _bm2m <- parseOptionalFloatParameter
  _bm2i <- parseOptionalFloatParameter
  _bm2d <- parseOptionalFloatParameter
  _bi2m <- parseOptionalFloatParameter
  _bi2i <- parseOptionalFloatParameter
  _bd2m <- parseOptionalFloatParameter
  _bd2d <- parseOptionalFloatParameter
  newline
  return (HMMER3Composite (V.fromList _matchEmissions),HMMER3Node (0 :: Int) V.empty (Just (0::Int)) Nothing Nothing False Nothing (V.fromList _insertEmissions) _bm2m _bm2i _bm2d _bi2m _bi2i _bd2m _bd2d)

-- | Parse HMMER3 node
parseHMMER3Node :: String -> GenParser Char st HMMER3Node
parseHMMER3Node alphabet = do
  _nodeId <- parseIntParameter
  _matchEmissions <- count (setEmissionNumber alphabet) (try parseDoubleParameter)
  _nma <- parseOptionalIntParameter
  _ncs <- parseOptionalCharParameter
  _nrf <- parseOptionalCharParameter
  _nmm <- parseMaskParameter
  _ncs <- parseOptionalStructureParameter
  newline
  _insertEmissions <- many1 parseDoubleParameter
  newline
  _m2m <- parseOptionalFloatParameter
  _m2i <- parseOptionalFloatParameter
  _m2d <- parseOptionalFloatParameter
  _i2m <- parseOptionalFloatParameter
  _i2i <- parseOptionalFloatParameter
  _d2m <- parseOptionalFloatParameter
  _d2d <- parseOptionalFloatParameter
  newline
  return $ HMMER3Node _nodeId (V.fromList _matchEmissions) _nma _ncs _nrf _nmm _ncs (V.fromList _insertEmissions) _m2m _m2i _m2d _i2m _i2i _d2m _d2d 

setEmissionNumber :: String -> Int
setEmissionNumber alphabet
  | alphabet == "DNA" = 4
  | alphabet == "dna" = 4 
  | alphabet == "RNA" = 4
  | alphabet == "rna" = 4
  | alphabet == "AMINO" = 20
  | alphabet == "amino" = 20
  | alphabet == "COINS" = 2
  | alphabet == "coins" = 2                        
  | alphabet == "DICE" = 6
  | alphabet == "dice" = 6                       
  | otherwise = 0

alphabetToSymbols :: String -> String
alphabetToSymbols  alphabet
  | alphabet == "DNA" = "ACGT"
  | alphabet == "dna" = "ACGT"
  | alphabet == "RNA" = "ACGU"
  | alphabet == "rna" = "ACGU"
  | alphabet == "AMINO" = "ACDEFGHIKLMNPQRSTVWY"
  | alphabet == "amino" = "ACDEFGHIKLMNPQRSTVWY"
  | alphabet == "COINS" = "HT"
  | alphabet == "coins" = "HT"
  | alphabet == "DICE" = "123456"          
  | alphabet == "dice" = "123456" 
  | otherwise = ""

parseDoubleParameter :: GenParser Char st Double
parseDoubleParameter = do
  many1 (oneOf " ")
  _float <- parseFloat
  return _float
  
parseAlphabetSymbol :: GenParser Char st Char
parseAlphabetSymbol = do
  many1 (try (oneOf " "))
  _symbol <- try (oneOf ("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  return _symbol

parseCharParameter :: GenParser Char st Char
parseCharParameter = do
  many1 (oneOf " ")
  _symbol <- upper
  return _symbol

parseIntParameter :: GenParser Char st Int
parseIntParameter = do
  many1 (oneOf " ")
  _int <- parseIntegral
  return _int

parseStructureParameter :: GenParser Char st Char
parseStructureParameter = do
  many1 (oneOf " ")
  _stru <- oneOf "<>().:[]{}~,_ABCDEFGHIJKLMNOPQRST"
  return _stru

parseOptionalCharParameter :: GenParser Char st (Maybe Char)
parseOptionalCharParameter = do
  many1 (oneOf " ")
  _symbol <- alphaNum <|> char '-' 
  return (optionalCharToMaybe _symbol)

parseOptionalIntParameter :: GenParser Char st (Maybe Int)
parseOptionalIntParameter = do
  many1 (oneOf " ")
  _int <- many1 digit
  return (optionalIntToMaybe _int)

parseOptionalFloatParameter :: GenParser Char st (Maybe Double)
parseOptionalFloatParameter = do
  many1 (oneOf " ")
  _float <- many1 (oneOf "1234567890.e-*")
  return (optionalFloatToMaybe _float)

parseMaskParameter :: GenParser Char st Bool
parseMaskParameter = do
  many1 (oneOf " ")
  _mask <- oneOf "m-"
  return (maskToBool _mask)

maskToBool :: Char -> Bool
maskToBool mask
  | mask == 'm' = True 
  | otherwise = False

optionalCharToMaybe :: Char -> Maybe Char
optionalCharToMaybe c
  | c == '-' = Nothing
  | otherwise = Just c

optionalIntToMaybe :: String -> Maybe Int
optionalIntToMaybe c
  | c == "-" = Nothing
  | otherwise = Just (read c :: Int)

optionalStructureToMaybe :: Char -> Maybe Char
optionalStructureToMaybe c
  | c == '-' = Nothing
  | otherwise = Just c 

optionalFloatToMaybe :: String -> Maybe Double
optionalFloatToMaybe c
  | c == "*" = Nothing
  | otherwise = Just (read c :: Double)

parseOptionalStructureParameter :: GenParser Char st (Maybe Char)
parseOptionalStructureParameter = do
  many1 (oneOf " ")
  _stru <- oneOf "<>().:[]{}~,_ABCDEFGHIJKLMNOPQRST-"
  return (optionalStructureToMaybe _stru)
