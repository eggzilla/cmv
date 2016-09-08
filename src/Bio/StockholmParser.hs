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
  many newline
  --per file annotations
  _fileAnnotations <- many genParseAnnotation
  many newline
  
  eof
  return $ Stockholm _version

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

