-- | 
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes #-}

module Bio.StockholmDraw
    (
     drawStockholmLines,
     drawStockholm,
     convertWUSStoDotBracket,
     extractGapfreeStructure,
     isGap
    ) where
  
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import qualified Bio.StockholmData as S
import qualified Data.Text as T    
import Data.Maybe
import qualified Data.Vector as V
import Data.List
import Graphics.SVGFonts
import Bio.CMFont

drawStockholmLines :: Int -> Double -> V.Vector Int -> V.Vector (Int, V.Vector (Colour Double)) -> S.StockholmAlignment -> QDiagram Cairo V2 Double Any
drawStockholmLines entriesNumberCutoff maxWidth nodeAlignmentColIndices comparisonNodeLabels aln = alignmentBlocks
  where currentEntries = V.fromList (take entriesNumberCutoff (S.sequenceEntries aln))
        entryNumber = V.length currentEntries
        vectorEntries = V.map makeVectorEntries currentEntries
        maxEntryLength = V.maximum (V.map (V.length . snd) vectorEntries)
        maxIdLength = V.maximum (V.map (length . fst) vectorEntries)
        letterWidth = (2.0 :: Double)
        availableLettersPerRow = maxWidth / letterWidth
        blocks = makeLetterIntervals entryNumber availableLettersPerRow maxEntryLength
        colIndicescomparisonNodeLabels = V.zipWith (\a b -> (a,b)) nodeAlignmentColIndices comparisonNodeLabels
        sparseComparisonColLabels = V.map nodeToColIndices colIndicescomparisonNodeLabels
        fullComparisonColLabels = fillComparisonColLabels maxEntryLength sparseComparisonColLabels
        alignmentBlocks = vcat' with { _sep = 6.0 } (map (drawStockholmRowBlock maxIdLength vectorEntries maxEntryLength fullComparisonColLabels) blocks)

extractGapfreeStructure :: String -> String -> String
extractGapfreeStructure alignedSequence regularStructure1 = entryStructure
  where regularsequence1 = map convertToRegularGap alignedSequence
        bpindicestest1 = basePairIndices regularStructure1 [] 0
        sequencegaps = elemIndices '-' regularsequence1
        -- convert incomplete basepairs to .
        incompleteBasepairs = filter (\(i,j) -> (elem i sequencegaps) || (elem j sequencegaps)) bpindicestest1
        incompleteIndicesCharacterPairs = concatMap (\(a,b) -> [(a,'.'),(b,'.')]) incompleteBasepairs
        completeBPStructure = V.update (V.fromList regularStructure1) (V.fromList incompleteIndicesCharacterPairs) 
        -- remove gap character postitions from structure string
        gapfreeCompleteStructure = V.filter (\(i,_) -> not (elem i sequencegaps)) (V.indexed completeBPStructure)
        entryStructure = map snd (V.toList  gapfreeCompleteStructure)
  
basePairIndices :: String -> [Int] -> Int -> [(Int,Int)]
basePairIndices (x:xs) ys counter
  | x == '(' = basePairIndices xs (counter:ys) (counter+1)
  | x == ')' = [(head ys,counter)] ++ basePairIndices xs (tail ys) (counter+1)
  | x == '.' = [] ++ (basePairIndices xs ys (counter+1))
  | otherwise = [] ++ (basePairIndices xs ys (counter+1))
basePairIndices [] _ _ = [] 

isGap :: Char -> Bool
isGap char
  | char == '.' = True
  | char == '-' = True
  | char == ' ' = True
  | char == '\n' = True
  | otherwise = False

convertToRegularGap :: Char -> Char
convertToRegularGap char 
  | char == '.' = '-'
  | char == ' ' = '-'
  | char == '\n' = '-'
  | otherwise = char

convertWUSStoDotBracket :: T.Text -> T.Text
convertWUSStoDotBracket wuss = T.pack $ map convertWUSSCharToDotBracket (T.unpack wuss)

convertWUSSCharToDotBracket :: Char -> Char
convertWUSSCharToDotBracket c
  | c == '<' = '('
  | c == '>' = ')'
  | c == '_' = '.'
  | c == '-' = '.'
  | c == '(' = '('
  | c == ')' = ')'
  | c == '.' = '.'
  | c == '[' = '('
  | c == ']' = ')'
  | c == '{' = '('
  | c == '}' = ')'
  | c == '~' = '.'
  | c == ':' = '.'
  | c == ',' = '.'
  | otherwise = c
  

nodeToColIndices :: (Int,(Int,V.Vector (Colour Double))) -> (Int,V.Vector (Colour Double))
nodeToColIndices (colIndex,(_,colors)) = (colIndex,colors)

fillComparisonColLabels :: Int ->  V.Vector (Int, V.Vector (Colour Double)) ->  V.Vector (Int, V.Vector (Colour Double))
fillComparisonColLabels maxEntryLength sparseComparisonColLabels = fullComparisonColLabels
   where fullComparisonColLabels = V.generate maxEntryLength  (makeFullComparisonColLabel sparseComparisonColLabels)

makeFullComparisonColLabel :: V.Vector (Int, V.Vector (Colour Double)) -> Int -> (Int, V.Vector (Colour Double))
makeFullComparisonColLabel sparseComparisonColLabels colIndex = fullComparisonColLabel
  where availableLabel = V.find (\(a,_)-> colIndex == a) sparseComparisonColLabels
        fullComparisonColLabel = if isJust availableLabel then fromJust availableLabel else (colIndex,V.singleton white)

drawStockholmRowBlock :: Int ->  V.Vector (String, V.Vector Char) -> Int -> V.Vector (Int, V.Vector (Colour Double)) -> ((Int, Int), V.Vector (Int, Int, Int)) -> QDiagram Cairo V2 Double Any
drawStockholmRowBlock maxIdLength vectorEntries maxEntryLength comparisonColLabels ((startIndex,endIndex),letterIntervals) = blockSequences
  where indices = [startIndex..safeEndIndex]        
        safeEndIndex = if (endIndex-1) > (maxEntryLength-1) then maxEntryLength-1 else endIndex-1
        indexLine = drawStockholmIndexLine maxIdLength indices comparisonColLabels     
        blockSequences = indexLine === strutY 2.0 === vcat' with { _sep = 2.0 } (V.toList (V.map (drawStockholmEntryLine maxIdLength vectorEntries) letterIntervals))

drawStockholmIndexLine :: Int -> [Int] -> V.Vector (Int, V.Vector (Colour Double)) -> QDiagram Cairo V2 Double Any
drawStockholmIndexLine maxIdLength indices comparisonColLabels = indexLine
  where --entryText = (spacer ++ indexLetters)      
        spacerLength = (maxIdLength + 3) 
        spacer = replicate spacerLength ' '
        --indexLetters = map show indices
        --indexPositions = maximum (map length indices)
        maxEntryIndex = maximum indices
        maxEntryText = show maxEntryIndex
        totalBoxYlength = fromIntegral (length  maxEntryText) * 2.5
        indexLine = hcat (map setAlignmentLetter spacer) ||| hcat (map (drawStockholmIndexLineCol comparisonColLabels totalBoxYlength) indices)

drawStockholmIndexLineCol :: V.Vector (Int, V.Vector (Colour Double)) -> Double -> Int -> QDiagram Cairo V2 Double Any
drawStockholmIndexLineCol comparisonColLabels totalBoxYlength entryIndex = vcat (map setAlignmentLetter entryText) <> colourBoxes # translate (r2 (0, negate ((singleBoxYLength/2)-1.25)))
  where comparisonColLabel = comparisonColLabels V.! entryIndex
        colColours = snd comparisonColLabel
        boxNumber = fromIntegral $ V.length colColours
        singleBoxYLength = totalBoxYlength / boxNumber
        entryText = show entryIndex
        colourBoxes = vcat (V.toList (V.map (colorBox singleBoxYLength) colColours))

colorBox :: Double -> Colour Double -> QDiagram Cairo V2 Double Any
colorBox singleBoxYLength colColour = rect 2 singleBoxYLength # fc colColour # lw 0.1

drawStockholmEntryLine :: Int -> V.Vector (String, V.Vector Char) -> (Int, Int, Int) -> QDiagram Cairo V2 Double Any
drawStockholmEntryLine maxIdLength aln (seqIndex,currentStart,safeLength) = entryDia
  where entry = aln V.! seqIndex
        entryText = (seqId ++ spacer ++ entrySeq)
        seqId = fst entry      
        entrySeq = V.toList (V.slice currentStart safeLength (snd entry))
        spacerLength = (maxIdLength + 3) - length seqId 
        spacer = replicate spacerLength ' '
        entryDia = hcat (map setAlignmentLetter entryText)

drawStockholm :: Int -> S.StockholmAlignment -> QDiagram Cairo V2 Double Any
drawStockholm entriesNumberCutoff aln = alignTL (vcat' with { _sep = 1 } (map (drawStockholmEntry maxIdLength) currentEntries))
   where currentEntries = take entriesNumberCutoff (S.sequenceEntries aln)
         --entryNumber = length currentEntries
         maxIdLength = maximum (map (T.length . S.sequenceId) currentEntries)

drawStockholmEntry :: Int -> S.SequenceEntry -> QDiagram Cairo V2 Double Any
drawStockholmEntry maxIdLength entry = entryDia
  where entryText = T.unpack (seqId `T.append` spacer `T.append` (S.entrySequence entry))         
        seqId = S.sequenceId entry             
        spacerLength = (maxIdLength + 3) - T.length seqId 
        spacer = T.replicate spacerLength (T.pack " ")
        entryDia = hcat (map setAlignmentLetter entryText)         

setAlignmentLetter :: Char -> QDiagram Cairo V2 Double Any
setAlignmentLetter echar = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False 2.5 2.5) [echar] # fc black # fillRule EvenOdd  # lw 0.0 # translate (r2 (negate 0.75, negate 0.75)) <> rect 2 2 # lw 0.0
--setAlignmentLetter echar = alignedText 0.5 0.5 [echar] # fontSize 2.0 <> rect 2 2.5 # lw 0

-- LetterInterval (SeqNr,Start,Length)
makeLetterIntervals :: Int -> Double -> Int -> [((Int,Int),V.Vector (Int,Int,Int))]
makeLetterIntervals seqNumber letterNumberPerRow letterNumber = rowIntervals
  where --rowVector = V.iterateN rowNumber (1+) 0
        rowList = [0..(rowNumber-1)]
        rowNumber = ceiling $ (fromIntegral letterNumber) / letterNumberPerRow
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
        rowLength = letterNumberPerRow 
	safeLength = if currentStart + rowLength >= letterNumber then (letterNumber - currentStart) else rowLength

makeVectorEntries :: S.SequenceEntry -> (String, V.Vector Char)
makeVectorEntries entry = (entrySeqId,entrySeq)
  where entrySeq = V.fromList (T.unpack (S.entrySequence entry))
        entrySeqId = T.unpack (S.sequenceId entry)
