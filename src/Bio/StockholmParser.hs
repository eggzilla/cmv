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
tokenToStockholm version token = StockholmAlignment version _fileAnnotation _columnAnnotation _sequenceEntries
  where _fileAtoken =  [x | TokFileA x <- token]
        _colAtoken =  [x | TokColA x <- token]
        _resAtoken =  [x | TokResA x <- token]
        _seqAtoken =  [x | TokSeqA x <- token]
        _seqtoken =  [x | TokSeq x <- token]
        _fileAnnotation = mergeFileToken _fileAtoken
        _columnAnnotation = mergeColToken _colAtoken
        mergedRAToken = mergeResAToken _resAtoken
        mergedSeqAToken = mergeSeqAToken _seqAtoken
        mergedSeqenceToken = mergeSeqToken _seqtoken
        _sequenceEntries = buildSeqEntries mergedRAToken mergedSeqAToken mergedSeqenceToken

mergeFileToken :: [TokFileA] -> [AnnotationEntry]
mergeFileToken token = entries
  where tags = nub (map fTag token)
        entries = map (buildFEntry token) tag

buildFEntry ::  [TokFileA] ->  T.Text -> AnnotationEntry
buildFEntry  token tag = entry
  where tagToken = filter (\t -> fTag t) token
        tagInfos = T.concat (map tInfo tagToken)
        entry = AnnotationEntry tag tagInfos

mergeColToken :: [TokColA] -> [AnnotationEntry]
mergeColToken token = entries
  where tags = nub (map cTag token)
        entries = map (buildCEntry token) tag

buildCEntry :: [TokColA] -> T.Text -> AnnotationEntry
buildCEntry token tag = entry
  where tagToken = filter (\t -> cTag t) token
        tagInfos = T.concatMap cInfo tagToken
        entry = AnnotationEntry tag tagInfos

mergeSeqAToken :: [TokSeqA] -> [TokSeqA]
mergeSeqAToken token = entries
  where aIds = nub (map aId token)
        entries = map (mergeSAIdtoken token) aIds

mergeSAIdtoken :: [TokSeqA] -> T.Text -> [TokSeqA]
mergeSAIdtoken token currentId = tagIdToken
  where idToken = filter (\t -> aId == currentid) token
        tags = nub (map aTag idToken)
        tagIdToken = concatMap (mergeSAIdTagToken idToken currentId) tags

mergeSAIdTagToken :: [TokSeqA] ->  T.Text -> T.Text -> TokSeqA
mergeSAIdTagToken token currentId currenttag= entry
  where tagToken = filter (\t -> aid t == currentId) token
        tagInfos = T.concatMap aInfo tagToken
        entry = TokSeqA currentid currenttag tagInfos   

mergeResAToken :: [TokResA] -> [TokResA]
mergeSeqAToken token = entries
  where rIds = nub (map rId token)
        entries = map (mergeRAIdtoken token) aIds

mergeRAIdtoken :: [TokResA] -> T.Text -> [TokResA]
mergeRAIdtoken token currentId = tagIdToken
  where idToken = filter (\t -> rId == currentid) token
        tags = nub (map rTag idToken)
        tagIdToken = concatMap (mergeRAIdTagToken idToken currentId) tags

mergeRAIdTagToken :: [TokResA] ->  T.Text -> T.Text -> TokResA
mergeRAIdTagToken token currentId currentTag= entry
  where tagToken = filter (\t -> rId t == currentId) token
        tagInfos = T.concatMap rInfo tagToken
        entry = TokSeqA currentId currentTag tagInfos 

mergeSeqToken :: [TokSeq] -> [TokResA] -> [TokSeqA] -> [SequenceEntry]
mergeSeqToken token resA seqA = entries
  where currentId = map sId token
        entries = map (buildEntry token resA seqA) currentId
         
buildSeqEntry :: [TokSeq] -> [TokResA] -> [TokSeqA] -> T.Text -> SequenceEntry
buildSeqEntry token resA seqA currentId = entry 
  where idToken = filter (\t -> sid t == currentId ) token
        idraToken = filter (\t -> rid t == currentId ) resA
        idsaToken = filter (\t -> aid t == currentId ) seqA
        tagInfos = T.concatMap tInfo idToken
        entry = SequenceEntry currentId tagInfos idsaToken idraToken

