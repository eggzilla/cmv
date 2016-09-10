-- | Parse Stockholm alignments
module Bio.StockholmParser (
                       parseStockholm,
                       readStockholm,
                       module Bio.StockholmData
                      ) where

import Bio.StockholmData
import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import qualified Control.Exception.Base as CE
import qualified Data.Text as T
import Data.List

-- | parse 
parseStockholm :: [Char] -> Either ParseError StockholmAlignment
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
  many1 (try (string " "))
  _version <- many1 (noneOf "\n")
  many (try newline)
  _stockholmToken <- many1 genParseToken
  string "//\n"
  eof
  return (tokenToStockholm (T.pack _version) _stockholmToken)

-- | Parse the input as StockholmAlignment datatype
genParseToken :: GenParser Char st StockholmToken
genParseToken = do
  tok <- choice [try genParseTokFileA, try genParseTokColA, try genParseTokResA, try genParseTokSeqA, try genParseTokSeq]
  return tok

genParseTokFileA :: GenParser Char st StockholmToken
genParseTokFileA = do
  many newline
  string "#=GF"
  char ' '
  _tag <- many1 upper
  many1 (char ' ')
  _info <- many1 (noneOf "\n")
  newline
  return $ (TokFileA (T.pack _tag) (T.pack _info))
  
genParseTokColA :: GenParser Char st StockholmToken
genParseTokColA = do
  many newline
  string "#=GC"
  char ' '
  _tag <- many1 (noneOf " \n")
  many1 (char ' ')
  _info <- many1 (noneOf "\n")
  newline         
  return $ TokColA (T.pack _tag) (T.pack _info)
           
genParseTokResA :: GenParser Char st StockholmToken
genParseTokResA = do
  many newline
  string "#=GR"
  char ' '
  _id <- many1 (noneOf " \n")
  many1 (char ' ')
  _tag <- many1 (noneOf " \n")
  _info <- many1 (noneOf "\n")
  newline         
  return $ TokResA (T.pack _id) (T.pack _tag) (T.pack _info)
  
genParseTokSeqA :: GenParser Char st StockholmToken
genParseTokSeqA = do
  many newline
  string "#=GS"
  char ' '
  _id <- many1 (noneOf " \n")
  many1 (char ' ')
  _tag <- many1 (noneOf " \n")
  _info <- many1 (noneOf "\n")
  return $ TokSeqA (T.pack _id) (T.pack  _tag) (T.pack _info)

genParseTokSeq :: GenParser Char st StockholmToken
genParseTokSeq = do
  many newline
  _sid <- many1 (noneOf " \n")
  many1 (char ' ')
  _sequence <- many1 (oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ-")
  newline             
  return $ TokSeq (T.pack _sid) (T.pack _sequence)

tokenToStockholm :: T.Text -> [StockholmToken] -> StockholmAlignment
tokenToStockholm version token = StockholmAlignment version _fileAnnotation _columnAnnotation _sequenceEntries
  where _fileAtoken = filter isFileTok token
        _colAtoken = filter isColATok token
        _resAtoken = filter isResATok token
        _seqAtoken = filter isSeqATok token
        _seqtoken = filter isSeqTok token
        _fileAnnotation = mergeFileToken _fileAtoken
        _columnAnnotation = mergeColToken _colAtoken
        mergedSeqAToken = mergeSeqAToken _seqAtoken                    
        mergedRAToken = mergeResAToken _resAtoken
        _sequenceEntries = buildSeqEntries mergedSeqAToken mergedRAToken _seqtoken

isFileTok :: StockholmToken -> Bool                           
isFileTok (TokFileA x y) = True
isFileTok _ = False

isColATok :: StockholmToken -> Bool              
isColATok (TokColA x y) = True
isColATok _ = False

isResATok :: StockholmToken -> Bool              
isResATok (TokResA x y z) = True
isResATok _ = False

isSeqATok :: StockholmToken -> Bool              
isSeqATok (TokSeqA x y z) = True
isSeqATok _ = False

isSeqTok :: StockholmToken -> Bool              
isSeqTok (TokSeq x y) = True
isSeqTok _ = False              
              
mergeFileToken :: [StockholmToken] -> [AnnotationEntry]
mergeFileToken token = entries
  where tags = nub (map fTag token)
        entries = map (buildFEntry token) tags

buildFEntry ::  [StockholmToken] ->  T.Text -> AnnotationEntry
buildFEntry  token currentTag = entry
  where tagToken = filter (\t -> fTag t == currentTag) token
        tagInfos = T.concat (map fInfo tagToken)
        entry = AnnotationEntry currentTag tagInfos

mergeColToken :: [StockholmToken] -> [AnnotationEntry]
mergeColToken token = entries
  where tags = nub (map cTag token)
        entries = map (buildCEntry token) tags

buildCEntry :: [StockholmToken] -> T.Text -> AnnotationEntry
buildCEntry token currentTag = entry
  where tagToken = filter (\t -> cTag t == currentTag) token
        tagInfos = T.concat (map cInfo tagToken)
        entry = AnnotationEntry currentTag tagInfos

mergeSeqAToken :: [StockholmToken] -> [StockholmToken]
mergeSeqAToken token = entries
  where aIds = nub (map aId token)
        entries = concatMap (mergeSAIdToken token) aIds

mergeSAIdToken :: [StockholmToken] -> T.Text -> [StockholmToken]
mergeSAIdToken token currentId = tagIdToken
  where idToken = filter (\t -> aId t == currentId) token
        tags = nub (map aTag idToken)
        tagIdToken = map (mergeSAIdTagToken idToken currentId) tags

mergeSAIdTagToken :: [StockholmToken] ->  T.Text -> T.Text -> StockholmToken
mergeSAIdTagToken token currentId currentTag = entry
  where tagToken = filter (\t -> aId t == currentId) token
        tagInfos = T.concat (map aInfo tagToken)
        entry = TokSeqA currentId currentTag tagInfos

mergeResAToken :: [StockholmToken] -> [StockholmToken]
mergeResAToken token = entries
  where rIds = nub (map rId token)
        entries = concatMap (mergeRAIdToken token) rIds

mergeRAIdToken :: [StockholmToken] -> T.Text -> [StockholmToken]
mergeRAIdToken token currentId = tagIdToken
  where idToken = filter (\t -> rId t == currentId) token
        tags = nub (map rTag idToken)
        tagIdToken = map (mergeRAIdTagToken idToken currentId) tags

mergeRAIdTagToken :: [StockholmToken] ->  T.Text -> T.Text -> StockholmToken
mergeRAIdTagToken token currentId currentTag= entry
  where tagToken = filter (\t -> rId t == currentId) token
        tagInfos = T.concat (map rInfo tagToken)
        entry = TokSeqA currentId currentTag tagInfos

buildSeqEntries :: [StockholmToken] -> [StockholmToken] -> [StockholmToken] -> [SequenceEntry]
buildSeqEntries  seqA resA token= entries
  where currentId = map sId token
        entries = map (buildSeqEntry seqA resA token) currentId
         
buildSeqEntry :: [StockholmToken] -> [StockholmToken] -> [StockholmToken] -> T.Text -> SequenceEntry
buildSeqEntry seqAtok resAtok token currentId = entry 
  where idToken = filter (\t -> sId t == currentId ) token
        idSAToken = filter (\t -> aId t == currentId ) seqAtok
        idRAToken = filter (\t -> rId t == currentId ) resAtok
        seqA = map buildSAEntry idSAToken
        resA = map buildRAEntry idRAToken      
        tagInfos = T.concat (map sSeq idToken)
        entry = SequenceEntry currentId tagInfos seqA resA

                
buildSAEntry :: StockholmToken -> AnnotationEntry
buildSAEntry tok = AnnotationEntry (aTag tok) (aInfo tok)

buildRAEntry :: StockholmToken -> AnnotationEntry
buildRAEntry tok = AnnotationEntry (rTag tok) (rInfo tok)

                   

