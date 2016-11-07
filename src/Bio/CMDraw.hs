-- | Drawing of covariance model (http://www.tbi.univie.ac.at/software/cmcompare/) guide trees and highlighting comparison results
-- Drawing is done with the diagrams package
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes #-}

module Bio.CMDraw
    (
     drawCMComparisons,
     drawSingleCMComparisons,
     drawCMs,
     drawSingleCMs,
     drawCM,
     text',
     svgsize,
     diagramName,
     printCM,
     processCMs,
     processCMGuideTree,
     getNodeInfo,
     sortModelsByComparisonResults,
     findModelError,
     findModel,
     findModelIndex,
     findMissing,
     getCMName,
     checkCMCResultsParsed,
     checkCMCResultParsed,
     checkSortedModels,
     getComparisonsHighlightParameters,
     getComparisonHighlightParameters,
     highlightComparisonIntervalBoundry,
     connectionLine,
     mkPath,
     NodeIndices,
     buildRowIndexStructure,
     buildTreeIndexStructure,
     getIndexEnd,
     secondaryStructureVisualisation
    ) where
  
import Diagrams.Prelude
import Data.Typeable.Internal
import Bio.CMCompareResult
import qualified Biobase.SElab.CM as CM
import Data.List
import Text.Parsec.Error
import qualified Data.Text as T
import qualified Data.Vector as V
import Bio.StockholmData
import Bio.StockholmDraw
import qualified Diagrams.Backend.Cairo as C
import Diagrams.Backend.Cairo
import qualified Data.Vector.Unboxed as VU
import qualified Data.PrimitiveArray.Index.PhantomInt as PI
import qualified Data.PrimitiveArray.Class as PA
import Biobase.SElab.Bitscore
import Biobase.Primary.Nuc.RNA
import qualified Data.PrimitiveArray.Index.Class as PAI
import Text.Printf
import Biobase.SElab.Types
import Data.Colour
import qualified Data.Colour.SRGB.Linear as R
import Data.Maybe
import GHC.Float
import Control.Monad.State
import qualified Data.Char as C
import Graphics.SVGFonts

-- | Draw one or more CM guide trees and concatenate them vertically
drawCMComparisons modelDetail entryNumberCutoff modelLayout emissiontype maxWidth cms alns comparisons = alignTL (vcat' with { _sep = 20 } (map (drawCM modelDetail entryNumberCutoff modelLayout emissiontype maxWidth) zippedInput))
  where zippedInput = zip4 cms alns comparisonNodeLabels (V.toList colorVector)
        modelNumber = length cms
        comparisonNodeLabels = map (getComparisonNodeLabels comparisons nameColorVector) cms
	colorVector = makeColorVector modelNumber
	modelNames = V.fromList (map (T.unpack . CM._name) cms)
	nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector
	-- comparisonsHighlightParameter = getComparisonsHighlightParameters cms comparisons

-- | Draw one or more CM 
drawSingleCMComparisons modelDetail entryNumberCutoff modelLayout emissiontype maxWidth cms alns comparisons = map (drawCM modelDetail entryNumberCutoff modelLayout emissiontype maxWidth) zippedInput
  where zippedInput = zip4 cms alns comparisonNodeLabels (V.toList colorVector)
        modelNumber = length cms
        comparisonNodeLabels = map (getComparisonNodeLabels comparisons nameColorVector) cms
	colorVector = makeColorVector modelNumber
	modelNames =  V.fromList (map (T.unpack . CM._name) cms)
	nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector
	comparisonsHighlightParameter = getComparisonsHighlightParameters cms comparisons

-- | Draw one or more CM and concatenate them vertically
drawCMs modelDetail entryNumberCutoff modelLayout emissiontype maxWidth cms alns = alignTL (vcat' with { _sep = 40 } (map (drawCM modelDetail entryNumberCutoff modelLayout emissiontype maxWidth) zippedInput))
    where zippedInput = zip4 cms alns comparisonNodeLabels colorList
          comparisonNodeLabels = map getBlankComparisonNodeLabels cms
          colorList = replicate (length cms) white

-- | Draw one or more CM 
drawSingleCMs modelDetail entryNumberCutoff modelLayout emissiontype maxWidth cms alns = map (drawCM modelDetail entryNumberCutoff modelLayout emissiontype maxWidth) zippedInput
    where zippedInput = zip4 cms alns comparisonNodeLabels colorList
          comparisonNodeLabels = map getBlankComparisonNodeLabels cms
          colorList = replicate (length cms) white

-- | Draw the guide Tree of a single CM
--drawCM :: forall n b. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => [Char] -> [(String, [Char])] -> QDiagram b V2 n Any
drawCM modelDetail entryNumberCutoff modelLayout emissiontype maxWidth (cm,aln,comparisonNodeLabels,modelColor)
   | modelLayout == "tree" = modelTreeLayout
   | modelLayout == "flat" = modelFlatLayout
   | otherwise = modelTreeLayout
   where nodes = CM._nodes cm
         nodeNumber = V.length nodes
         allStates = CM._states cm
         boxlength = fromIntegral (length alphabetSymbols) + 2
         alphabetSymbols = ['A','U','C','G']
	 nodeAlignmentColIndices = V.map (CM._nColL) nodes
         indices = V.toList (V.iterateN (nodeNumber-1) (1+) 0)
         (indexStructure,finalState)= runState (buildTreeIndexStructure 1 nodes indices) startState 
         --indexStructureGroupedByParent = groupBy indexStructureParent (fst indexStructure)
	 --nullModel = CM._nullModel cm
	 --nullModelBitscores = VU.toList (VU.map (getBitscore) nullModel)
	 --dummyLetters = replicate (length nullModelBitscores) "I"
	 --dummyNullModelBitscores = zip dummyLetters nullModelBitscores
	 --nullModelBox = vcat (map (emissionEntry "score") dummyNullModelBitscores)
	 modelName = CM._name cm
	 modelFlatLayout = alignTL (vcat' with {_sep=5} [modelHeader,detailedNodeTransitions,alignmentDiagram])
         modelTreeLayout = alignTL (vcat' with {_sep=5} [modelHeader,detailedNodeTreeTransitions,alignmentDiagram])
         detailedNodeTreeTransitions = applyAll (arrowList ++ labelList) detailedNodesTree
         firstInterval = fromJust (find (\(_,p,_,_,_) -> p == 0) (fst indexStructure))
	 detailedNodesTree = drawCMNodeTree modelDetail alphabetSymbols emissiontype boxlength allStates comparisonNodeLabels nodes (fst indexStructure) firstInterval
	 modelHeader = makeModelHeader (T.unpack modelName) modelColor
	 nodeIndices = V.iterateN nodeNumber (1+) 0
	 detailedNodeTransitions = applyAll (arrowList ++ labelList) detailedNodes
	 detailedNodes = vcat (V.toList (V.map (drawCMNode modelDetail alphabetSymbols emissiontype boxlength (0 :: Int) nodeNumber nodeNumber allStates comparisonNodeLabels nodes) nodeIndices))
         trans = CM._sTransitions (CM._states cm)
         (lo,up) = PA.bounds trans
         (transitionIndexTuple,transitionPAIndices) = unzip $ makeTransitionIndices (deConstr up)
         transitionBitscores = map ((\(a,b) ->((show (PI.getPInt a)),(score2Prob 1 b))) . (trans PA.!)) transitionPAIndices
         allConnectedStates = V.map (\((stateId,targetId),(targetStateIndex,bitS)) -> (stateId,targetStateIndex,bitS,(0,0))) (V.fromList (zip transitionIndexTuple transitionBitscores))
	 connectedStates = V.filter (\(stateId,targetStateIndex,_,_) -> stateId /= targetStateIndex) allConnectedStates
	 selfConnectedStates = V.filter (\(stateId,targetStateIndex,_,_) -> stateId == targetStateIndex) allConnectedStates
	 arrowList = case modelDetail of
                          "detailed" -> V.toList (V.map makeArrow connectedStates V.++ V.map makeSelfArrow selfConnectedStates)
                          "interval"-> map (makeArrow . indexStructureToConnections) (filter (\(acc,emit,_,_,_)-> acc /= emit)(fst indexStructure))
                          _ -> []                        
         labelList = case modelDetail of
                          "detailed" -> V.toList (V.map makeLabel connectedStates V.++ V.map makeSelfLabel selfConnectedStates)
                          _ -> []
	 alignmentDiagram = if isJust aln then drawStockholmLines entryNumberCutoff maxWidth nodeAlignmentColIndices comparisonNodeLabels (fromJust aln) else mempty

-- | Extracts consensus secondary structure from alignment and annotates cmcompare nodes
secondaryStructureVisualisation selectedTool maxWidth cms alns comparisons
  | selectedTool == "forna" = fornaVis
  | selectedTool == "r2r" = r2rVis
  | otherwise = []
  where fornaVis = map buildFornaInput structureComparisonInfo
        r2rVis = map buildR2RInput structureComparisonInfo
	modelNumber = length cms
	comparisonNodeLabels = map (getComparisonNodeLabels comparisons nameColorVector) cms
	colorVector = makeColorVector modelNumber
	modelNames = V.fromList (map (T.unpack . CM._name) cms)
	nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector
	structureComparisonInfo = zip3 cms alns comparisonNodeLabels

buildFornaInput (cm,maybeAln,comparisonNodeLabels)
  | isNothing maybeAln = ([],[])
  | otherwise = (fornaInput, colorScheme)
  where aln = fromJust maybeAln
        fornaInput = ">" ++ modelName ++ "\n" ++ gapfreeConsensusSequence ++ "\n" ++ consensusStructure
        allColumnAnnotations = columnAnnotations aln
        consensusSequenceList = map annotation (filter (\annotEntry -> tag annotEntry == T.pack "RF") allColumnAnnotations)
	consensusSequence = if null consensusSequenceList then "" else T.unpack (head consensusSequenceList)
        gapfreeConsensusSequence = map C.toUpper (filter (not . isGap) consensusSequence)
	consensusStructureList = map (convertWUSStoDotBracket . annotation) (filter (\annotEntry -> tag annotEntry == T.pack "SS_cons") allColumnAnnotations)
	consensusStructure = if null consensusStructureList then "" else extractGapfreeStructure consensusSequence (T.unpack (head consensusStructureList))
        modelName = T.unpack $ CM._name cm
        nodes = CM._nodes cm
        nodeAlignmentColIndices = V.map (CM._nColL) nodes
	maxEntryLength = length consensusStructure
        colIndicescomparisonNodeLabels = V.zipWith (\a b -> (a,b)) nodeAlignmentColIndices comparisonNodeLabels
        sparseComparisonColLabels = V.map nodeToColIndices colIndicescomparisonNodeLabels
        fullComparisonColLabels = fillComparisonColLabels maxEntryLength sparseComparisonColLabels
        --forna only supports a single color per node, which has to be supplied as additional color scheme
        singleColorLabels = concatMap comparisonColLabelsToFornaLabel (V.toList fullComparisonColLabels)
        colorScheme = singleColorLabels

comparisonColLabelsToFornaLabel (nodeNr,colorVector)
  | V.length colorVector > 1 =  " " ++ show nodeNr ++ ":red "
  | otherwise = ""
                
buildR2RInput (cm,maybeAln,comparisonNodeLabels)
  | isNothing maybeAln = ([],[])
  | otherwise = (r2rInput,[])
  where aln = fromJust maybeAln
        r2rInput = sHeader ++ sConsensusStructure ++ sConsensusSequence ++ sConsensusSequenceColor ++ sCovarianceAnnotation ++ sComparisonHighlight ++ sBackboneColorLabel
        allColumnAnnotations = columnAnnotations aln
        consensusSequenceList = map annotation (filter (\annotEntry -> tag annotEntry == T.pack "RF") allColumnAnnotations)
	consensusSequence = if null consensusSequenceList then "" else T.unpack (head consensusSequenceList)
	gapfreeConsensusSequence = map C.toUpper (filter (not . isGap) consensusSequence)
	consensusStructureList = map (convertWUSStoDotBracket . annotation) (filter (\annotEntry -> tag annotEntry == T.pack "SS_cons") allColumnAnnotations)
	consensusStructure = if null consensusStructureList then "" else extractGapfreeStructure consensusSequence (T.unpack (head consensusStructureList))
        modelName = T.unpack $ CM._name cm
        nodes = CM._nodes cm
	maxEntryLength = length consensusStructure
        nodeAlignmentColIndices = V.map (CM._nColL) nodes
        colIndicescomparisonNodeLabels = V.zipWith (\a b -> (a,b)) nodeAlignmentColIndices comparisonNodeLabels
        sparseComparisonColLabels = V.map nodeToColIndices colIndicescomparisonNodeLabels
        fullComparisonColLabels = fillComparisonColLabels maxEntryLength sparseComparisonColLabels
        r2rLabels = map comparisonColLabelsToR2RLabel (V.toList fullComparisonColLabels)
        sHeader =  "# STOCKHOLM 1.0\n"
        sConsensusStructure =     "#=GC SS_cons          " ++ consensusStructure ++ "\n"
        sConsensusSequence =      "#=GC cons             " ++ gapfreeConsensusSequence ++ "\n"
        sConsensusSequenceColor = "#=GC conss            " ++ (replicate (length consensusStructure) '2') ++ "\n"
        sCovarianceAnnotation =   "#=GC cov_SS_cons      " ++ (replicate (length consensusStructure) '.') ++ "\n"
        sComparisonHighlight =    "#=GC R2R_LABEL        " ++ r2rLabels ++ "\n"
        sBackboneColorLabel =     "#=GF R2R shade_along_backbone s rgb:200,0,0\n"

comparisonColLabelsToR2RLabel (nodeNr,colorVector)
  | length colorVector > 1 = 's'
  | otherwise = '.'

nodeToColIndices :: (Int,(Int,V.Vector (Colour Double))) -> (Int,V.Vector (Colour Double))
nodeToColIndices (colIndex,(nodeIndex,colors)) = (colIndex,colors)

fillComparisonColLabels :: Int ->  V.Vector (Int, V.Vector (Colour Double)) ->  V.Vector (Int, V.Vector (Colour Double))
fillComparisonColLabels maxEntryLength sparseComparisonColLabels = fullComparisonColLabels
   where fullComparisonColLabels = V.generate maxEntryLength (makeFullComparisonColLabel sparseComparisonColLabels)

makeFullComparisonColLabel sparseComparisonColLabels colIndex = fullComparisonColLabel
  where availableLabel = V.find (\(a,c)-> colIndex == a) sparseComparisonColLabels
        fullComparisonColLabel = if isJust availableLabel then fromJust availableLabel else (colIndex,V.singleton white)

indexStructureToConnections (acc,emit,_,_,_) = (show emit,show acc,1,(0,0))

indexStructureRow (row1,_,_,_,_)  (row2,_,_,_,_) = row1 == row2
indexStructureParent (_,p1,_,_,_)  (_,p2,_,_,_) = p1 == p2

data NodeIndices = S [Int] | L [Int] | R [Int]
  deriving (Show, Eq, Ord)
         
startState = ([],0::Int)
--type IndexState =  [(Int,Int,String,Int,Int)]
buildRowIndexStructure :: Int -> V.Vector CM.Node -> [Int] -> State ([(Int,Int,String,Int,Int)],Int) ([(Int,Int,String,Int,Int)],Int)
buildRowIndexStructure row nodes [] = do
  (a,b) <- get
  return (a,b)

buildRowIndexStructure row nodes (currentIndex:xs) = do
  (interval,parentId) <- get
  let currentNode = nodes V.! currentIndex
  let currentEnd = getIndexEnd nodes (currentIndex:xs)
  let ntype = CM._ntype currentNode
  case ntype of                  
    CM.NodeType 6 -> put (((row,parentId,"S,",currentIndex,currentEnd):interval),parentId) -- ROOT start tree             
    CM.NodeType 4 -> put (((row,parentId,"L,",currentIndex,currentEnd):interval),parentId) --(L [currentIndex..currentEnd],buildIndexStructure nodes remainingIndices) -- BEGL set current label
    CM.NodeType 5 -> put (((row,parentId,"R,",currentIndex,currentEnd):interval),parentId) -- (R [currentIndex..currentEnd],buildIndexStructure nodes remainingIndices)  -- BEGR set current label
    CM.NodeType 1 -> put (interval,parentId)
    CM.NodeType 2 -> put (interval,parentId)
    CM.NodeType 3 -> put (interval,parentId)
    CM.NodeType 7 -> put (interval,parentId)
    CM.NodeType 8 -> put (interval,parentId)
    CM.NodeType 9 -> put (interval,parentId)
    CM.NodeType 10 -> put (interval,parentId)
    CM.NodeType 0 -> put (interval,parentId+1)
  buildRowIndexStructure row nodes xs

buildTreeIndexStructure :: Int -> V.Vector CM.Node -> [Int] -> State ([(Int,Int,String,Int,Int)],Int) ([(Int,Int,String,Int,Int)],Int)                      
buildTreeIndexStructure row nodes [] = do
  (a,b) <- get
  return (a,b)
                      
buildTreeIndexStructure intervalId nodes (currentIndex:xs) = do
  (interval,parentId) <- get
  let currentNode = nodes V.! currentIndex
  let currentEnd = getIndexEnd nodes (currentIndex:xs)
  let ntype = CM._ntype currentNode
  let maxId = if null interval then 0 else maximum $ map (\(iid,_,_,_,_)-> iid) interval
  let newId = maxId +1
  let nextId = setNextId ntype intervalId newId
  case ntype of                  
    CM.NodeType 6 -> put (((intervalId,parentId,"S,",currentIndex,currentEnd):interval),parentId) 
    CM.NodeType 4 -> put (((newId,parentId,"L,",currentIndex,currentEnd):interval),parentId) 
    CM.NodeType 5 -> put (((newId,parentId,"R,",currentIndex,currentEnd):interval),parentId)
    CM.NodeType 1 -> put (interval,parentId)
    CM.NodeType 2 -> put (interval,parentId)
    CM.NodeType 3 -> put (interval,parentId)
    CM.NodeType 7 -> put (interval,parentId)
    CM.NodeType 8 -> put (interval,parentId)
    CM.NodeType 9 -> put (interval,parentId)
    CM.NodeType 10 -> put (interval,parentId)
    CM.NodeType 0 -> put (interval,intervalId)
  buildTreeIndexStructure nextId nodes xs

setNextId ntype intervalId newId
  | ntype == CM.NodeType 6 = newId
  | ntype == CM.NodeType 4 = newId
  | ntype == CM.NodeType 5 = newId
  | otherwise = intervalId
                      
getIndexEnd nodes indices
  | null indices = (length nodes -1)
  | ntype == CM.NodeType 7 = currentIndex
  | ntype == CM.NodeType 0 = currentIndex
  | otherwise = getIndexEnd nodes remainingindices
   where currentIndex = head indices
         remainingindices = tail indices
         currentNode = nodes V.! currentIndex
         ntype = CM._ntype currentNode
                            
makeModelHeader mName modelColor = strutX 2 ||| hcat (map setTitelLetter mName) ||| strutX 1 ||| rect 4 4 # lw 0.1 # fc modelColor
setLabelLetter echar = alignedText 0.5 0.5 [echar] # fontSize 0.75 <> rect 0.4 0.5 # lw 0
setStateLetter echar = alignedText 1 1 [echar] # fontSize 2.0 <> rect 2.0 2.0 # lw 0
setTitelLetter echar = alignedText 0.5 0.5 [echar] # fontSize 4.0 <> rect 4.0 4.0 # lw 0

drawCMNodeTree modelDetail alphabetSymbols emissiontype boxlength allStates comparisonNodeLabels nodes indexStructure (intervalId,parentId,intervalType,currentIndex,currentEnd) = nodeTree
  where nodeTree = currentIntervalDrawing === hcat' with {_sep = 2} (map (drawCMNodeTree modelDetail alphabetSymbols emissiontype boxlength allStates comparisonNodeLabels nodes indexStructure) nextIntervals)
        nextIntervals = filter (\(_,p,_,_,_) -> intervalId == p) indexStructure
        currentIntervalDrawing = drawCMNodeInterval modelDetail alphabetSymbols emissiontype boxlength currentIndex currentEnd currentEnd allStates comparisonNodeLabels nodes (intervalId,parentId,intervalType,currentIndex,currentEnd) -- ||| (text' (show intervalId ++ "I" ++ show indexStructure) <> rect 100 100)
 
drawCMNodeRow modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes intervals = strutY 4 === hcat' with { _sep = 8 } (map (drawCMNodeInterval modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes) intervals)

drawCMNodeInterval modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes (intervalId,parent,intervaltype,currentIndex,currentEnd)
  | modelDetail == "interval" = intervalVis
  | otherwise = nodeVis
  where intervalVis = rect 20 0 # named ("a" ++ intervalIdString)  # lw 0.0 === (rect 20 40 # lw 0.1 <> text' ((show currentIndex) ++ "-" ++ (show currentEnd))) === rect 20 0 # named ("e" ++ intervalIdString)  # lw 0.0 === strutY 5.0
        intervalIdString = show intervalId
        nodeVis = vcat' with { _sep = nodespacer } (map (drawCMNode modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes) currentInterval)
        currentInterval = [currentIndex..currentEnd]
        nodespacer = if (modelDetail == "detailed") then (2 :: Double) else (0 :: Double)

getCMNodeType node
  | ntype == CM.NodeType 0 = "BIF"
  | ntype == CM.NodeType 1 = "MATP"
  | ntype == CM.NodeType 2 = "MATL"
  | ntype == CM.NodeType 3 = "MATR"
  | ntype == CM.NodeType 4 = "BEGL"
  | ntype == CM.NodeType 5 = "BEGR"
  | ntype == CM.NodeType 6 = "ROOT"
  | ntype == CM.NodeType 7 = "END"
  | otherwise = "NA"
    where ntype = CM._ntype node


text'  t = textSVG_ (TextOpts lin INSIDE_H KERN False 3 3) t # fc black # fillRule EvenOdd  # lw 0.0
--text' t = alignedText 0.5 0.5 t # fontSize 2 <> rect textLength 2 # lw 0.0
--  where textLength = fromIntegral (length t) * 2
textWithSize' t si = alignedText 0.5 0.5 t # fontSize si <> rect textLength 2 # lw 0.0
  where textLength = fromIntegral (length t) * 2                     

-- | Transform covariance model node labels to colors
labelToColor :: forall b. (Floating b, Ord b) => [Char] -> Colour b
labelToColor label
   | label == "MATP" = sRGB24 211 211 211 -- P
   | label == "MATL" = sRGB24 211 211 211 -- L
   | label == "MATR" = sRGB24 211 211 211 -- R
   | label == "BIF"  = sRGB24 255 069 064 -- B
   | label == "ROOT" = sRGB24 245 245 245 -- S
   | label == "BEGL" = sRGB24 211 211 211 -- S
   | label == "BEGR" = sRGB24 211 211 211 -- S 
   | label == "END"  = sRGB24 245 245 245 -- E
labelToColor _ = sRGB24 245 245 245

--drawCMNodeDetailed :: [Char] -> String -> Double -> Int -> Int -> Int -> V.Vector (Int, V.Vector (Colour Double)) -> CM.States -> CM.Node -> QDiagram b V2 n Any
drawCMNode modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes nodeIndex
  | modelDetail == "minimal" = (alignedText 0 0 nId # fontSize 2 # translate (r2 ((negate ((fromIntegral (length nId))/2)), negate 1.25)) <> wheel nodeLabels # lw 0.1 <> rect 1.5 3 # lw 0.1) 
  | modelDetail == "simple" = ((text' nId # translate (r2 (negate 5,0)) <> colourBoxes # translate (r2 (negate 5, negate ((singleBoxYLength/2)-2.5)))) ||| strutX 2 ||| text' nodeType ) <> rect 20 5 # lw 0.5  
  | otherwise = detailedNodeBox
  where node = nodes V.! nodeIndex
        idNumber = PI.getPInt (CM._nid node)
        nId = show idNumber
	detailedNodeBox = drawCMNodeBox alphabetSymbols emissiontype boxlength states comparisonNodeLabels node
        nodeType = getCMNodeType node
        nodeLabels = V.toList (snd (comparisonNodeLabels V.! idNumber))
        boxNumber = fromIntegral $ length nodeLabels
        totalBoxYlength = 5 - 0.2
        singleBoxYLength = totalBoxYlength / boxNumber 
        colourBoxes = vcat (map (colorBox singleBoxYLength) nodeLabels) -- <> rect 5 totalBoxYlength # lw 0.1

colorBox singleBoxYLength colColour = rect 5 singleBoxYLength # fc colColour # lw 0.1
        

drawCMNodeBox alphabetSymbols emissiontype boxlength currentStates comparisonNodeLabels node
  | ntype == CM.NodeType 0 = bifNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.NodeType 1 = matPNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.NodeType 2 = matLNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.NodeType 3 = matRNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.NodeType 4 = begLNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.NodeType 5 = begRNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.NodeType 6 = rootNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.NodeType 7 = endNode # translate (r2 (negate 25,25)) <> nodeBox
  | otherwise = endNode <> nodeBox
    where ntype = CM._ntype node
          idNumber = PI.getPInt (CM._nid node)
          nId = show idNumber
	  nodeLabels = V.toList (snd (comparisonNodeLabels V.! idNumber))
          stateIndices = VU.toList (CM._nstates node)               
          splitStatesBox = hcat' with { _sep = 0.01 } (map (drawCMSplitStateBox nId alphabetSymbols emissiontype boxlength currentStates) stateIndices)
	  insertStatesBox = hcat (map (drawCMInsertStateBox nId alphabetSymbols emissiontype boxlength currentStates) stateIndices)
          -- bif b
          bifNode = ((idBox nId "BIF" nodeLabels) # rotate (1/4 @@ turn) # translate (r2 (0, negate 10)) ||| strutX 0.5 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- matP mp ml mr d il ir
          matPNode = ((idBox nId "MATP" nodeLabels) # rotate (1/4 @@ turn) # translate (r2 (0, negate 10)) ||| strutX 0.5||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- matL ml d il
          matLNode = ((idBox nId "MATL" nodeLabels) # rotate (1/4 @@ turn) # translate (r2 (0, negate 10)) ||| strutX 0.5 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- matR mr d ir
          matRNode = ((idBox nId "MATR" nodeLabels) # rotate (1/4 @@ turn) # translate (r2 (0, negate 10)) ||| strutX 0.5||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- begL s
          begLNode = ((idBox nId "BEGL" nodeLabels) # rotate (1/4 @@ turn) # translate (r2 (0, negate 10)) ||| strutX 0.5 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- begR s il
          begRNode = ((idBox nId "BEGR" nodeLabels) # rotate (1/4 @@ turn) # translate (r2 (0, negate 10)) ||| strutX 0.5||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- root s il ir
          rootNode = ((idBox nId "ROOT" nodeLabels) # rotate (1/4 @@ turn) # translate (r2 (0, negate 10)) ||| strutX 0.5||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- end e
          endNode = ((idBox nId "END" nodeLabels) # rotate (1/4 @@ turn) # translate (r2 (0, negate 10)) ||| strutX 0.5 ||| splitStatesBox) === strutY 5.0 === insertStatesBox

idBox nId nType nodeLabels = (alignedText 0 0 nId # fontSize 2 # translate (r2 ((negate ((fromIntegral (length nId))/2)), negate 1.25)) <>  wheel nodeLabels # lw 0.1 <> rect 1.5 3 # lw 0) ||| strutX 2.0 ||| text' nType
nodeBox = rect 60 60 # lw 0.1

wheel colors = wheel' # rotate r
   where
     wheel' = mconcat $ zipWith fc colors (iterateN n (rotate a) w)
     n = length colors
     a = 1 / (fromIntegral n) @@ turn
     w = wedge 3 xDir a # lwG 0
     r = (1/4 @@ turn)  ^-^  (1/(2*(fromIntegral n)) @@ turn)

--drawCMStateBox :: [Char]  -> String -> Double -> CM.States -> PI.PInt () CM.StateIndex -> QDiagram NullBackend V2 n Any
drawCMSplitStateBox nid alphabetSymbols emissiontype boxlength currentStates sIndex
  | stype == CM.StateType 0 = dState # translate (r2 (negate 3,negate 3)) <> statebox 8.0 20.0 stateIndx 
  | stype == CM.StateType 1 = mpState # translate (r2 (negate 7,negate 3)) <> statebox 16.0 20.0 stateIndx
  | stype == CM.StateType 2 = mlState # translate (r2 (negate 3,negate 3)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.StateType 3 = mrState # translate (r2 (negate 3,negate 3)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.StateType 6 = sState # translate (r2 (negate 3,negate 3)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.StateType 7 = eState # translate (r2 (negate 3,negate 3)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.StateType 8 = bState # translate (r2 (negate 3,negate 3)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.StateType 9 = elState # translate (r2 (negate 3,negate 3)) <> statebox 8.0 20.0 stateIndx
  | otherwise = mempty
    where stype = (CM._sStateType currentStates) PA.! sIndex
          stateIndx = show (PI.getPInt sIndex)
          singleEmissionBitscores = V.map ((score2Prob 1) . ((CM._sSingleEmissions currentStates) PA.!)) (makeSingleEmissionIndices sIndex)
          singleEmissionEntries = setEmissions emissiontype singleEmissionBitscores
          singleSymbolsAndEmissions = zip ["A","U","G","C"] (V.toList singleEmissionEntries)
	  pairEmissionBitscores = V.map ((score2Prob 1). ((CM._sPairEmissions currentStates) PA.!)) (makePairEmissionIndices sIndex)
          pairEmissionEntries = setEmissions emissiontype pairEmissionBitscores
	  pairSymbolsAndEmissions = zip ["AA","AU","AG","AC","UU","UA","UG","UC","GG","GA","GU","GC","CC","CA","CU","CG"] (V.toList pairEmissionEntries)
	  pairSymbolsAndEmissions1 = take 8 pairSymbolsAndEmissions
	  pairSymbolsAndEmissions2 = drop 8 pairSymbolsAndEmissions
          dState = textWithSize' ("D" ++ stateIndx) 1.5  # translate (r2 (3,0.5)) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          mpState = textWithSize' ("MP" ++ stateIndx) 1.5 # translate (r2 (7,0.5)) === strutY 1 === (vcat (map (emissionEntry emissiontype) pairSymbolsAndEmissions1) ||| strutX 0.5 ||| vcat (map (emissionEntry emissiontype) pairSymbolsAndEmissions2))
          mlState = textWithSize' ("ML" ++ stateIndx) 1.5 # translate (r2 (3,0.5)) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          mrState = textWithSize' ("MR" ++ stateIndx) 1.5 # translate (r2 (3,0.5)) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          sState = textWithSize' ("S" ++ stateIndx) 1.5 # translate (r2 (3,0.5)) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions)
          eState = textWithSize' ("E" ++ stateIndx) 1.5 # translate (r2 (3,0.5)) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          bState = textWithSize' ("B" ++ stateIndx) 1.5 # translate (r2 (3,0.5)) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          elState = textWithSize' ("EL" ++ stateIndx) 1.5 # translate (r2 (3,0.5)) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          
drawCMInsertStateBox nid alphabetSymbols emissiontype boxlength currentStates sIndex
  | stype == CM.StateType 4 = ((ilState # translate (r2 (negate 3,negate 2))) <> statebox 8.0 20.0 stateIndx) ||| strutX 38
  | stype == CM.StateType 5 = (irState # translate (r2 (negate 3,negate 2))) <> inverseStatebox 8.0 20.0 stateIndx   
  | otherwise = mempty
    where stype = (CM._sStateType currentStates) PA.! sIndex
          stateIndx = show (PI.getPInt sIndex)
          singleEmissionBitscores = V.map ((score2Prob 1.0) . ((CM._sSingleEmissions currentStates) PA.!)) (makeSingleEmissionIndices sIndex)
          singleEmissionEntries = setEmissions emissiontype singleEmissionBitscores
          singleSymbolsAndEmissions = zip ["A","U","G","C"] (V.toList singleEmissionEntries)
          ilState = textWithSize' ("IL" ++ stateIndx) 1.5 # translate (r2 (3,0.5)) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          irState = textWithSize' ("IR" ++ stateIndx) 1.5 # translate (r2 (3,0.5)) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 

setEmissions :: String -> V.Vector Double -> V.Vector Double
setEmissions emissiontype emissions
  | emissiontype == "score" = scoreentries
  | emissiontype == "probability" = propentries
  | emissiontype == "bar" = barentries
  | otherwise = barentries
    where scoreentries = emissions      
          propentries = V.map (exp . negate) emissions
          barentries = V.map (exp . negate) emissions

wrap x = [x]
         
--emissionEntry :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => String -> (String,Double) -> QDiagram b V2 n Any
emissionEntry emissiontype (symbol,emission) 
  | emissiontype == "probability" = textentry
  | emissiontype == "score" = textentry
  | emissiontype == "bar" = barentry
  | otherwise = barentry
    where textentry = alignedText 0 0.1 (symbol ++ " " ++ printf "%.3f" emission) # translate (r2 (negate 0.5,0)) <> (rect 3 2 # lw 0 ) 
          barentry = (alignedText 0 0.01 symbol  # translate (r2 (negate 0.5,negate 0.3)) <> (rect 3 2 # lw 0)) ||| bar emission

--bar :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => Double -> QDiagram b V2 n Any
bar emission = (rect (4 * emission) 1 # lw 0 # fc black # translate (r2 (negate (2 - (4 * emission/2)),0)) <> rect 4 1 # lw 0.03 )
                    
makeSingleEmissionIndices index = V.fromList [(PAI.Z  PAI.:. index PAI.:. A),(PAI.Z  PAI.:. index PAI.:. U),(PAI.Z  PAI.:. index PAI.:. G),(PAI.Z  PAI.:. index PAI.:. C)]

makePairEmissionIndices index = V.fromList [(PAI.Z  PAI.:. index PAI.:. A PAI.:. A),(PAI.Z  PAI.:. index PAI.:. A PAI.:. U),(PAI.Z  PAI.:. index PAI.:. A PAI.:. G),(PAI.Z  PAI.:. index PAI.:. A PAI.:. C),(PAI.Z  PAI.:. index PAI.:. U PAI.:. U),(PAI.Z  PAI.:. index PAI.:. U PAI.:. A),(PAI.Z  PAI.:. index PAI.:. U PAI.:. G),(PAI.Z  PAI.:. index PAI.:. U PAI.:. C),(PAI.Z  PAI.:. index PAI.:. G PAI.:. G),(PAI.Z  PAI.:. index PAI.:. G PAI.:. A),(PAI.Z  PAI.:. index PAI.:. G PAI.:. U),(PAI.Z  PAI.:. index PAI.:. G PAI.:. C),(PAI.Z  PAI.:. index PAI.:. C PAI.:. C),(PAI.Z  PAI.:. index PAI.:. C PAI.:. A),(PAI.Z  PAI.:. index PAI.:. C PAI.:. U),(PAI.Z  PAI.:. index PAI.:. C PAI.:.G)]

statebox x y si = ((rect 0.05 0.1 # lw 0 # named ("s" ++ si) ||| rect 1 0.1 # lw 0 # named ("a" ++ si) ||| rect 2 0.1 # lw 0 ||| rect 0.05 0.1 # lw 0 # named ("z" ++ si))) === rect x y  # lw 0.1 === rect 1 0 # lw 0 # named ("e" ++ si)

inverseStatebox x y si = ((rect 0.05 0.1 # lw 0 # named ("s" ++ si) ||| rect 1 0.1 # lw 0 # named ("a" ++ si) ||| rect 2 0.1 # lw 0  ||| rect 0.05 0.1 # lw 0 # named ("z" ++ si))) === rect x y  # lw 0.1 === rect 1 0 # lw 0 # named ("e" ++ si)                  
                  
--scaling
-- | Specifies the size of the diagram. Absolute adapts to overall size according to subdiagrams
svgsize :: SizeSpec V2 Double
svgsize = mkSizeSpec2D Nothing Nothing
           
-- | Check for available cairo output formats
diagramName :: String -> String -> Either String String
diagramName filename fileformat
  | fileformat == "pdf" = Right (filename ++ "." ++ fileformat )
  | fileformat == "svg" = Right (filename ++ "." ++ fileformat )
  | fileformat == "png" = Right (filename ++ "." ++ fileformat )
  | fileformat == "ps" = Right (filename ++ "." ++ fileformat )
  | otherwise = Left "Unsupported output format requested (use svg, pdf, ps, png)"

printCM outputName = renderCairo outputName
                
processCMs :: [CM.CM] -> [[(String,String)]]
processCMs cms = map processCMGuideTree cms

processCMGuideTree :: CM.CM -> [(String,String)]
--processCMGuideTree cm = map getNodeInfo (Map.assocs (CM._nodes cm))
processCMGuideTree cm = map getNodeInfo (V.toList (CM._nodes cm))

getBlankComparisonNodeLabels :: CM.CM -> V.Vector (Int, V.Vector (Colour Double))
getBlankComparisonNodeLabels model = comparisonNodeLabels
   where comparisonNodeLabels = V.generate (nodeNumber +1 ) makeBlankComparisonNodeLabel
         nodeNumber = CM._nodesInModel model

makeBlankComparisonNodeLabel :: Int ->  (Int,(V.Vector (Colour Double)))
makeBlankComparisonNodeLabel nodeNumber = (nodeNumber,V.singleton white)

getComparisonNodeLabels :: [CmcompareResult] -> V.Vector (String, Colour Double) -> CM.CM -> V.Vector (Int, V.Vector (Colour Double))
getComparisonNodeLabels comparsionResults colorVector model = comparisonNodeLabels
   where modelName = T.unpack (CM._name model)
         relevantComparisons1 = filter ((modelName==) . model1Name) comparsionResults
	 modelNodeInterval1 = map (\a -> (model2Name a,model2matchednodes a))  relevantComparisons1 
	 relevantComparisons2 = filter ((modelName==) . model2Name) comparsionResults
         modelNodeInterval2 = map (\a -> (model1Name a,model1matchednodes a))  relevantComparisons2
         modelNodeIntervals =  V.fromList (modelNodeInterval1 ++ modelNodeInterval2)
         colorNodeIntervals = V.map (modelToColor colorVector) modelNodeIntervals
         nodeNumber = CM._nodesInModel model
         comparisonNodeLabels = V.generate (nodeNumber +1) (makeComparisonNodeLabel colorNodeIntervals)
         --nodeColorLabels = map model colorNodeIntervals

modelToColor :: V.Vector (String,Colour Double) ->  (String,[Int]) -> (Colour Double,[Int])
modelToColor colorVector (mName,nInterval) = nColorInterval
  where nColorInterval = (snd (fromJust entry),nInterval)
        --nColorInterval = maybe Nothing (\a -> Just (snd a,nInterval)) entry
        entry = V.find (\(a,c)-> mName == a) colorVector

makeComparisonNodeLabel :: V.Vector (Colour Double,[Int]) -> Int -> (Int,(V.Vector (Colour Double)))
makeComparisonNodeLabel colorNodeIntervals nodeNumber = comparisonNodeLabel
  where relevantColorNodeIntervals = V.filter (\(_,b) -> elem nodeNumber b) colorNodeIntervals
        modelColors = V.map fst relevantColorNodeIntervals
        comparisonNodeLabel = if (null modelColors) then (nodeNumber,V.singleton white) else (nodeNumber,modelColors)

makeColorVector modelNumber = V.map (\(a,b,c) -> R.rgb a b c) modelRGBTupel
   where indexVector = V.iterateN (modelNumber) (1+) 0
         stepSize = (765 :: Double) / (fromIntegral modelNumber)
	 modelRGBTupel = V.map (makeRGBTupel stepSize) indexVector

--makeRGBTupel :: Double -> Int -> (Double,Double,Double)
makeRGBTupel stepSize modelNumber = (a,b,c)
  where  totalSize = (fromIntegral modelNumber) * stepSize 
         a = (rgbBoundries (totalSize  - 255))/255
	 b = (rgbBoundries (totalSize - a - 255))/255
	 c = (rgbBoundries (totalSize - a - b))/255

--rgbBoundries :: Double -> Double	     
rgbBoundries rgbValue
  | rgbValue>210 = 210
  | rgbValue<50 = 50
  | otherwise = rgbValue

-- deConsSnd :: ((PAI.Z PAI.:. PInt () CM.StateIndex) PAI.:. Int) (PInt () CM.StateIndex, Bitscore) -> String
deConstr (PAI.Z PAI.:. a PAI.:. b) = PI.getPInt a 

--getPhantom (PInt i p) = p
-- fromIS PAI.IndexStream a = a

makeTransitionIndices n = concatMap makeTransitionSubIndices indexes
  where indexes = [0..n]

makeTransitionSubIndices n = map  (\subI -> ((show (n),show (n+subI)),(PAI.Z PAI.:. (PI.PInt n) PAI.:. subI ))) subIndices
  where subIndices = [0..4]

makeArrow (lab1,lab2,weight,_) = connectOutside' arrowStyle1 ("e" ++ lab1) ("a" ++ lab2) 
  where arrowStyle1 = with & arrowHead .~ spike & shaftStyle %~ lw (local (0.1)) & headLength .~ local 0.01 & shaftStyle %~ dashingG [weight, 0.1] 0 & headStyle %~ fc black . opacity (weight * 2)

makeSelfArrow (lab1,lab2,weight,_) = connectPerim' arrowStyle ("s" ++ lab1) ("z" ++ lab1) (5/12 @@ turn) (8/12 @@ turn)
  where arrowStyle = with  & arrowHead .~ spike & arrowShaft .~ shaft' & arrowTail .~ lineTail & tailTexture .~ solid black  & shaftStyle %~ lw (local (0.1)) & headLength .~ local 0.01  & tailLength .~ 0 & shaftStyle %~ dashingG [weight, 0.1] 0 & headStyle %~ fc black . opacity (weight * 2)
        shaft' = wedge 3 xDir (2/4 @@ turn) 

--  where arrowStyle = with  & arrowHead .~ spike & arrowShaft .~ shaft' & arrowTail .~ lineTail & tailTexture .~ solid black  & shaftStyle %~ lw (local (0.1)) & headLength .~ local 0.001  & tailLength .~ 0 & shaftStyle %~ dashingG [weight, 0.1] 0 & headStyle %~ fc black . opacity (weight * 2)
--        shaft' = arc xDir (-3.5/5 @@ turn) 

-- %~ lw (local (0.1 * weight))

makeLabel (n1,n2,weight,(xOffset,yOffset))=
  withName ("a" ++ n1) $ \b1 ->
  withName ("s" ++ n2) $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v ^/ 2)
    in
      Diagrams.Prelude.atop (position [((midpoint # translateX (negate 0.25 + xOffset) # translateY (0 + yOffset)), (hcat (map setLabelLetter (show (roundPos 2 weight)))))])

makeSelfLabel (n1,n2,weight,(xOffset,yOffset))
  | weight == 0 = mempty
  | otherwise = withName ("e" ++ n1) $ \b1 ->
                withName ("e" ++ n2) $ \b2 ->
                  let v = location b2 .-. location b1
                      midpoint = location b1 .+^ (v ^/ 2)
                  in
                    Diagrams.Prelude.atop (position [(midpoint # translateX (negate 0.25 + xOffset) # translateY (0 + yOffset), (hcat (map setLabelLetter (show (roundPos 2 weight)))))])

-------- Simple/ Flat functions
--getNodeInfo :: (CM.Node, (CM.NodeType, [CM.State])) -> (String,String)
--getNodeInfo (nodeid, (nodetype, _ )) = (show (CM._nId nodeid) , (show nodetype))

getNodeInfo :: CM.Node -> (String,String)
getNodeInfo _node = (show (CM._nid _node) , show (CM._ntype _node))

sortModelsByComparisonResults :: [String] -> [CM.CM] -> [Either String CM.CM]
sortModelsByComparisonResults cmComparisonNames models = map (\x -> findModelError x (findModel x models)) cmComparisonNames
-- todo: also add models at end of the sorted list that are not in comparisons
-- ++ (map (\x -> findModelError x (findMissing x models)) cmComparisonNames)

findModelError :: String -> Maybe CM.CM -> Either String CM.CM
findModelError _name (Just model) = Right model
findModelError _name Nothing = Left ("Model " ++ _name ++ "that is present in comparison file is not present in model file")

findModel :: String -> [CM.CM] -> Maybe CM.CM
findModel check models = find (\x -> getCMName x == check) models

findModelIndex :: String -> [CM.CM] -> Maybe Int
findModelIndex check models = findIndex (\x -> getCMName x == check) models

findMissing :: String -> [CM.CM] -> Maybe CM.CM
findMissing check models = find (\x -> getCMName x /= check) models

getCMName :: CM.CM -> String
getCMName x = filter (\c -> c /= ' ')  (T.unpack (CM._name x))

checkCMCResultsParsed :: [ParseError] -> IO ()
checkCMCResultsParsed x 
  | null x = print "Parsing comparisons - done\n"
  | otherwise = error ("Following errors occured:" ++ (concat (map checkCMCResultParsed x)))

checkCMCResultParsed :: ParseError -> String
checkCMCResultParsed x = (concat (map messageString (errorMessages x)))
--  | (errorIsUnknown x) = print "Parsing comparisons - done\n" 
--  | x == [] = print "Parsing comparisons - done\n"
--  | otherwise = (concat (map messageString (errorMessages x)))
--  | otherwise = error ("Following errors occured :" ++ (concat (map (\y -> (concat (map messageString (errorMessages y)))) x)))

checkSortedModels :: forall a. (Eq a, Show a) => [[a]] -> IO ()
checkSortedModels x
  | x == [] = print "Sorting input models to comparison list - done\n" 
  | otherwise = error ("Following errors occured :" ++ show (concat x))

getComparisonsHighlightParameters :: [CM.CM] -> [CmcompareResult] -> [(Int,Int,Int,Int,Int,Int,Int,Int)]
getComparisonsHighlightParameters sortedmodels comp = map (getComparisonHighlightParameters sortedmodels) comp

getComparisonHighlightParameters :: [CM.CM] -> CmcompareResult -> (Int,Int,Int,Int,Int,Int,Int,Int)
getComparisonHighlightParameters sortedmodels comp = (a,b,c,d,a,f,c,e)
  where --a = (fromJust (findModelIndex (model1Name comp) sortedmodels) + 1)
        a = 1
        b = head (model1matchednodes comp)
        --c = (fromJust (findModelIndex (model2Name comp) sortedmodels) + 1)
        c = 2
        d = head (model2matchednodes comp)
        e = last (model2matchednodes comp)
        f = last (model1matchednodes comp)

-- | Highlight comparison by connecting the delimiting nodes of the aligned nodes of both models
-- takes the model identifier of both models and the starting and ending nodes of both models as arguments.
--highlightComparisonLines a b c d e f g h = highlightComparisonIntervalBoundry a b c d <> highlightComparisonIntervalBoundry e f g h
highlightComparisonIntervalBoundry :: forall b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2, N b ~ Double) => Int -> Int -> Int -> Int -> b
highlightComparisonIntervalBoundry model1index node1index model2index node2index = connectionLine (getNodeCoordinates "detailed" model1index node1index) (getNodeCoordinates "detailed" model2index node2index)

highlightComparisonTrails :: forall b. Renderable (Path V2 Double) b => String -> [(Int, Int, Int, Int, Int, Int, Int, Int)] -> [QDiagram b V2 Double Any]
highlightComparisonTrails modelDetail trails  = map (highlightComparisonTrail modelDetail) trails

-- | Highlight comparison by connecting the all of the aligned nodes of both models
highlightComparisonTrail :: forall b. Renderable (Path V2 Double) b => String -> (Int, Int, Int, Int, Int, Int, Int, Int) -> QDiagram b V2 Double Any
highlightComparisonTrail modelDetail (a,b,c,d,e,f,g,h) = connectionTrail (getNodeCoordinates modelDetail a b) (getNodeCoordinates modelDetail c d) (getNodeCoordinates  modelDetail e f) (getNodeCoordinates modelDetail g h)

-- | Returns the center coordinates for an Covariance model guide tree node
getNodeCoordinates :: String -> Int -> Int -> P2 Double
getNodeCoordinates modelDetail modelindex nodeindex 
   | modelDetail == "simple" = p2 (fromIntegral x, fromIntegral y)
   | modelDetail == "flat" = p2 (fromIntegral a, fromIntegral b)
   | otherwise = p2 (fromIntegral x, fromIntegral y)
      where y = (getYCoordinateDetailed modelindex 0) * (-1)
            x = (5 + (10 * (nodeindex - 1 )))
            a = (1 + (2 * (nodeindex - 1 )))
            b = (getYCoordinateSimple modelindex 0) * (-1)


-- |  Computes the y coodinate for comparison highlighting, so that the
-- line or area starts at the lower edge of the cm representation and ends right above it
getYCoordinateDetailed :: Int -> Int -> Int 
getYCoordinateDetailed modelindex ycoordinate
    | modelindex == 0 = ycoordinate 
    | even modelindex = getYCoordinateDetailed (modelindex - 1) (ycoordinate + 40) 
    | otherwise = getYCoordinateDetailed (modelindex - 1) (ycoordinate + 10)
    

getYCoordinateSimple :: Int -> Int -> Int 
getYCoordinateSimple modelindex ycoordinate
    | modelindex == 0 = ycoordinate 
    | even modelindex = getYCoordinateSimple (modelindex - 1) (ycoordinate + 8) 
    | otherwise = getYCoordinateSimple (modelindex - 1) (ycoordinate + 2) 

connectionLine :: forall b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => Point (V b) (N b) -> Point (V b) (N b) -> b
connectionLine a b = fromVertices [a,b] # lw 0.5 # lc green

connectionTrail :: forall b n. (RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => Point V2 n -> Point V2 n -> Point V2 n -> Point V2 n -> QDiagram b V2 n Any
connectionTrail a b c d = stroke (paralellogram a b c d ) # fc aqua # fillRule EvenOdd # lc black # lw 0.1

mkPath :: forall a. (Num (N a), Monoid a, Semigroup a, Additive (V a), HasOrigin a) => (Point (V a) (N a), a) -> (Point (V a) (N a), a) -> (Point (V a) (N a), a) -> (Point (V a) (N a), a) -> a
mkPath a b c d = position [a,b,c,d,a]
              
paralellogram :: forall (v :: * -> *) n. (Floating n, Ord n, Metric v) => Point v n -> Point v n -> Point v n -> Point v n -> Path v n
paralellogram a b c d = pathFromTrailAt (closeTrail (trailFromVertices [a,b,d,c,a])) a

roundPos :: (RealFrac a) => Int -> a -> a
roundPos positions number  = (fromInteger $ round $ number * (10^positions)) / (10.0^^positions)
