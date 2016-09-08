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

StockholmToken = StockSeq | FileA | ColA | SeqA | Seq | ResA

