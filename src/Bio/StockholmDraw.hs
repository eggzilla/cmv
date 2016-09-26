-- | 
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes #-}

module Bio.StockholmDraw
    (
     drawStockholmLines,
     drawStockholm
    ) where
  
import Diagrams.Prelude
import qualified Diagrams.Backend.Cairo as C
import Data.Typeable.Internal
import Text.Printf
import GHC.Float
import qualified Bio.StockholmData as S
import qualified Data.Text as T    
import Data.Maybe
import qualified Data.Vector as V
import Bio.HMMCompareResult


drawStockholmLines entriesNumberCutoff maxWidth comparisonNodeLabels aln = alignmentBlocks
  where currentEntries = V.fromList (take entriesNumberCutoff (S.sequenceEntries aln))
        entryNumber = V.length currentEntries
        vectorEntries = V.map makeVectorEntries currentEntries
        maxEntryLength = V.maximum (V.map (V.length . snd) vectorEntries)
        maxIdLength = V.maximum (V.map (length . fst) vectorEntries)
        headerLength =  (fromIntegral maxIdLength)  + 3 * letterWidth
        letterWidth = (2.0 :: Double)
        availableLettersPerRow = (maxWidth -  headerLength) / letterWidth
        rowNumber = floor (availableLettersPerRow / (fromIntegral maxEntryLength))
        blocks = makeLetterIntervals entryNumber availableLettersPerRow maxEntryLength
        --alignmentRows = vcat' with { _sep = 6.0 } (V.toList (V.map (drawStockholmEntryLine maxIdLength vectorEntries) letterIntervals))
        alignmentBlocks = vcat' with { _sep = 6.0 } (map (drawStockholmRowBlock maxIdLength vectorEntries) blocks)

drawStockholmRowBlock maxIdLength vectorEntries ((startIndex,endIndex),letterIntervals) = blockSequences
  where indices = [startIndex..(endIndex-1)]
        indexLine = drawStockholmIndexLine maxIdLength indices        
        blockSequences = indexLine === strutY 2.0 === vcat' with { _sep = 2.0 } (V.toList (V.map (drawStockholmEntryLine maxIdLength vectorEntries) letterIntervals))

drawStockholmIndexLine maxIdLength indices = indexLine
  where --entryText = (spacer ++ indexLetters)      
        spacerLength = (maxIdLength + 3) 
        spacer = replicate spacerLength ' '
        indexLetters = map show indices
        indexPositions = maximum (map length indexLetters)
        indexLine =  hcat (map setAlignmentLetter spacer) ||| hcat (map drawStockholmIndexLineCol indexLetters)

drawStockholmIndexLineCol entryText = vcat (map setAlignmentLetter entryText) 


                    
drawStockholmEntryLine maxIdLength aln (seqIndex,start,safeLength) = entryDia
  where entry = aln V.! seqIndex
        entryText = (seqId ++ spacer ++ entrySeq)
        seqId = fst entry      
        entrySeq = V.toList (V.slice start safeLength (snd entry))
        spacerLength = (maxIdLength + 3) - length seqId 
        spacer = replicate spacerLength ' '
        entryDia = hcat (map setAlignmentLetter entryText) 

drawStockholm entriesNumberCutoff aln = alignTL (vcat' with { _sep = 1 } (map (drawStockholmEntry maxIdLength) currentEntries))
   where currentEntries = take entriesNumberCutoff (S.sequenceEntries aln)
         entryNumber = length currentEntries
         maxIdLength = maximum (map (T.length . S.sequenceId) currentEntries)

drawStockholmEntry maxIdLength entry = entryDia
  where entryText = T.unpack (seqId `T.append` spacer `T.append` (S.entrySequence entry))         
        seqId = S.sequenceId entry             
        spacerLength = (maxIdLength + 3) - T.length seqId 
        spacer = T.replicate spacerLength (T.pack " ")
        entryDia = hcat (map setAlignmentLetter entryText)         

setAlignmentLetter echar = alignedText 0.5 0.5 [echar] # fontSize 2.0 <> rect 2 2.5 # lw 0

-- LetterInterval (SeqNr,Start,Length)
makeLetterIntervals :: Int -> Double -> Int -> [((Int,Int),V.Vector (Int,Int,Int))]
makeLetterIntervals seqNumber letterNumberPerRow letterNumber = rowIntervals
  where --rowVector = V.iterateN rowNumber (1+) 0
        rowList = [0..(rowNumber-1)]
        rowNumber = ceiling $ (fromIntegral letterNumber) / letterNumberPerRow
	--rowIntervals = V.concat (map (setAlignmentInterval (floor letterNumberPerRow) letterNumber seqNumber)  rowList)
        rowIntervals = map (setAlignmentInterval (floor letterNumberPerRow) letterNumber seqNumber)  rowList
        
setAlignmentInterval :: Int -> Int -> Int -> Int -> ((Int,Int),V.Vector (Int,Int,Int))
setAlignmentInterval letterNumberPerRow letterNumber seqNumber rowIndex = ((indexStart,indexEnd),seqLines)
  where seqVector = V.iterateN seqNumber (1+) 0
        seqLines = V.map (setAlignmentLineInterval letterNumberPerRow letterNumber rowIndex) seqVector
        indexStart = rowIndex * letterNumberPerRow
        indexEnd = indexStart + letterNumberPerRow
setAlignmentLineInterval :: Int -> Int -> Int -> Int -> (Int,Int,Int)
setAlignmentLineInterval letterNumberPerRow letterNumber rowIndex seqIndex = (seqIndex,currentStart,safeLength)
  where currentStart = rowIndex * letterNumberPerRow
        length = letterNumberPerRow 
	safeLength = if currentStart +length >= letterNumber then (letterNumber - currentStart) else length

makeVectorEntries :: S.SequenceEntry -> (String, V.Vector Char)
makeVectorEntries entry = (entrySeqId,entrySeq)
  where entrySeq = V.fromList (T.unpack (S.entrySequence entry))
        entrySeqId = T.unpack (S.sequenceId entry)
