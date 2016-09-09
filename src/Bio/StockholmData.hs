-- | Datastructures for Stockholm alignments

module Bio.StockholmData where

import qualified Data.Text as T

-- | Data structure for a Stockholm format alignment
data StockholmAlignment = StockholmAlignment
  {
    version :: T.Text,
    --annotations with the same tag are merged
    fileAnnotation :: [AnnotationEntry],
    columnAnnotation :: [AnnotationEntry],
    sequenceEntry :: SequenceEntry
    
  }
  deriving (Show, Eq)
  
SequenceEntry = SequenceEntry
  {
    sequenceId ::T.Text ,
    sequence :: T.Text,
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

StockholmToken =  TokFileA | TokColA | TokResA | TokSeq 


TokFileA = TokFileA
  {
  ftag :: T.Text,
  finfo :: T.Text
  }
  deriving (Show, Eq)

TokColA = TokColA
  {
  ctag :: T.Text,
  cinfo :: T.Text
  }
  deriving (Show, Eq)

TokResA = TokResA
  {
  tid :: T.Text,
  tag :: T.Text
  }
  deriving (Show, Eq)

TokSeqA = TokSeqA
  {
  aid :: T.Text,
  atag :: T.Text,
  ainfo :: T.Text        
  }
  deriving (Show, Eq)

TokSeq = TokSeq
  {
  sid :: T.Text,
  sseq :: T.Text
  }
  deriving (Show, Eq)
