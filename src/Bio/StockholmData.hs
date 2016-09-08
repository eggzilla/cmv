-- | Datastructures for Stockholm alignments

module Bio.StockholmData where

import qualified Data.Text as T

-- | Data structure for Stockholm
data Stockholm = Stockholm
  {
    -- File format version; mandatory
    version :: T.Text,
    fileAnnotation :: [AnnotationEntry],
    columnAnnotation :: [AnnotationEntry],
    sequenceEntry :: SequenceEntry
    
  }
  deriving (Show, Eq)
  
SequenceEntry = SequenceEntry
  {
    sequenceId ::T.Text ,
    sequence :: T.Text,
    sequenceAnnotation :: T.Text,
    residueAnnotation :: [AnnotationEntry]
  }
  deriving (Show, Eq)

AnnotationEntry = AnnotationEntry
  {
    tag:: T.Text,
    annotation :: T.Text
  }
  deriving (Show, Eq)
