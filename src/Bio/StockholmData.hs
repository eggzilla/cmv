-- | Datastructures for Stockholm alignments

module Bio.StockholmData where

import qualified Data.Text as T

-- | Data structure for a Stockholm format alignment
data StockholmAlignment = StockholmAlignment
  {
    version :: T.Text,
    --annotations with the same tag are merged
    fileAnnotations :: [AnnotationEntry],
    columnAnnotations :: [AnnotationEntry],
    sequenceEntries :: [SequenceEntry]
    
  }
  deriving (Show, Eq)
  
SequenceEntry = SequenceEntry
  {
    sequenceId :: T.Text,
    entrySequence :: T.Text,
    --annotations with the same tag are merged
    sequenceAnnotation :: [AnnotationEntry],
    residueAnnotation :: [AnnotationEntry]
  }
  deriving (Show, Eq)

AnnotationEntry = AnnotationEntry
  {
    tag:: T.Text,
    annotation :: T.Text
  }
  deriving (Show, Eq)

--StockholmToken = StockSeq | FileA | ColA | SeqA | Seq | ResA

StockholmToken =  TokFileA | TokColA | TokResA | TokSeqA | TokSeq 


TokFileA = TokFileA
  {
  fTag :: T.Text,
  fInfo :: T.Text
  }
  deriving (Show, Eq)

TokColA = TokColA
  {
  cTag :: T.Text,
  cInfo :: T.Text
  }
  deriving (Show, Eq)

TokResA = TokResA
  {
  rId :: T.Text,
  rTag :: T.Text
  }
  deriving (Show, Eq)

TokSeqA = TokSeqA
  {
  aId :: T.Text,
  aTag :: T.Text,
  aInfo :: T.Text        
  }
  deriving (Show, Eq)

TokSeq = TokSeq
  {
  sId :: T.Text,
  sSeq :: T.Text
  }
  deriving (Show, Eq)
