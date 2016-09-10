-- | Datastructures for Stockholm alignments

module Bio.StockholmData where

import qualified Data.Text as T

-- | Data structure for a Stockholm format alignment
data StockholmAlignment = StockholmAlignment
  { version :: T.Text,
    --annotations with the same tag are merged
    fileAnnotations :: [AnnotationEntry],
    columnAnnotations :: [AnnotationEntry],         
    sequenceEntries :: [SequenceEntry]  
  }
  deriving (Show, Eq)
  
data SequenceEntry = SequenceEntry
  {
    sequenceId :: T.Text,
    entrySequence :: T.Text,
    sequenceAnnotation :: [AnnotationEntry],
    residueAnnotation :: [AnnotationEntry]
  }
  deriving (Show, Eq)

data AnnotationEntry = AnnotationEntry
  {
    tag :: T.Text,
    annotation :: T.Text
  }
  deriving (Show, Eq)

data StockholmToken =  TokFileA{ fTag :: T.Text, fInfo :: T.Text } | TokColA { cTag :: T.Text, cInfo :: T.Text  } | TokResA {rId :: T.Text, rTag :: T.Text, rInfo :: T.Text} | TokSeqA {aId :: T.Text, aTag :: T.Text, aInfo :: T.Text} | TokSeq {sId :: T.Text, sSeq :: T.Text} deriving (Show, Eq)
                    
