-- | Parse Stockholm alignments
module Bio.HMMParser (
                       parseStockholm,
                       readStockholm
                       module Bio.Stockholm
                      ) where

import Bio.HMMData
import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import qualified Control.Exception.Base as CE

-- | parse 
parseStockholm :: [Char] -> Either ParseError Stockholm
parseStockholm input = parse genParseStockholm "Stockholm" input

-- | parse StockholmAlignment from input filePath                      
readStockholm :: String -> IO (Either ParseError StockholmAlignment)                  
readStockholm filePath = do 
  parsedFile <- parseFromFile genParseStockholm filePath
  CE.evaluate parsedFile

-- | Parse the input as StockholmAlignment
genParseStockholm :: GenParser Char st StockholmAlignment
genParseStockholm = do
  string "# STOCKHOLM"
  many1 string " "
  _version <- many1 (noneOf "\n")
  --per file annotations
  _stockholmToken <- many genParseToken
  many newline
  string "//"
  eof
  return (tokenToStockholm version token)

-- | Parse the input as StockholmAlignment datatype
genParseToken :: GenParser Char st Token
genParseToken = do
  tok <- choice [try genParseTokFileA, try genParseTokColA, try genParseTokResA, try genParseTokSeqA, try genParseTokSeq, try (string "\n")]
  return tok

genParseTokFileA :: GenParser Char st TokFileA
genParseTokFileA = do
  string "#=GF"
  char ' '
  _tag <- many1 upper
  many1 (char ' ')
  _info <- many1 (noneOf "\n")
  return TokFileA _tag _info
  
genParseTokColA :: GenParser Char st TokColA
genParseTokColA =
  string "#=GC"
  char ' '
  _tag <- many1 (noneOf " \n")
  many1 (char ' ')
  _info <- many1 (noneOf "\n")
  return TokColA _tag _info
           
genParseTokResA :: GenParser Char st TokResA
genParseTokResA = do
  string "#=GR"
  char ' '
  _id <- many1 (noneOf " \n")
  many1 (char ' ')
  _tag <- many1 (noneOf " \n")
  _info <- many1 (noneOf "\n")
  return TokColA _id _tag _info
  
genParseTokSeqA :: GenParser Char st TokSeqA
genParseTokSeqA = do
  string "#=GS"
  char ' '
  _id <- many1 (noneOf " \n")
  many1 (char ' ')
  _tag <- many1 (noneOf " \n")
  _info <- many1 (noneOf "\n")
  return TokColA _id _tag _info

genParseTokSeq :: GenParser Char st TokSeq
genParseTokSeq = do
  _sid <- many1 (noneOf " \n")
  many1 (char ' ')
  _sequence <- many1 (oneOf "SNYRUAGCT-")
  return TokSeq 

-- | Parse the input as StockholmAlignment datatype
genParseAnnotation :: GenParser Char st AnnotationEntry
genParseAnnotation = do
  string "# STOCKHOLM"
  many1 string " "
  _version <- many1 (noneOf "\n")
  newline
  newline
  --per file annotations
  _fileAnnoations <- many parseFileAnnotation
  eof
  return $ Stockholm _version

tokenToStockholm :: T.Text -> [StockholmToken] -> StockholmAlignment
tokenToStockholm version token = StockholmAlignment version [] [] (SequenceEntry T.empty T.empty [] [])
