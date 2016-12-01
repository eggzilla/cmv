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
import Graphics.SVGFonts.ReadFont (loadFont')
import qualified Data.Text as T 

-- | Draw one or more CM guide trees and concatenate them vertically
drawCMComparisons :: String -> Int -> String -> String -> Double -> [CM.CM] -> [(Maybe StockholmAlignment)] -> [CmcompareResult] -> QDiagram Cairo V2 Double Any
drawCMComparisons modelDetail entryNumberCutoff modelLayout emissiontype maxWidth cms alns comparisons = alignTL (vcat' with { _sep = 20 } (map (drawCM modelDetail entryNumberCutoff modelLayout emissiontype maxWidth) zippedInput))
  where zippedInput = zip4 cms alns comparisonNodeLabels (V.toList colorVector)
        modelNumber = length cms
        comparisonNodeLabels = map (getComparisonNodeLabels comparisons nameColorVector) cms
	colorVector = makeColorVector modelNumber
	modelNames = V.fromList (map (T.unpack . CM._name) cms)
	nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector
	-- comparisonsHighlightParameter = getComparisonsHighlightParameters cms comparisons

-- | Draw one or more CM
drawSingleCMComparisons :: String -> Int -> String -> String -> Double -> [CM.CM] -> [(Maybe StockholmAlignment)] -> [CmcompareResult] -> [QDiagram Cairo V2 Double Any]
drawSingleCMComparisons modelDetail entryNumberCutoff modelLayout emissiontype maxWidth cms alns comparisons = map (drawCM modelDetail entryNumberCutoff modelLayout emissiontype maxWidth) zippedInput
  where zippedInput = zip4 cms alns comparisonNodeLabels (V.toList colorVector)
        modelNumber = length cms
        comparisonNodeLabels = map (getComparisonNodeLabels comparisons nameColorVector) cms
	colorVector = makeColorVector modelNumber
	modelNames =  V.fromList (map (T.unpack . CM._name) cms)
	nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector
	comparisonsHighlightParameter = getComparisonsHighlightParameters cms comparisons

-- | Draw one or more CM and concatenate them vertically
drawCMs :: String -> Int -> String -> String -> Double -> [CM.CM] -> [(Maybe StockholmAlignment)] -> QDiagram Cairo V2 Double Any
drawCMs modelDetail entryNumberCutoff modelLayout emissiontype maxWidth cms alns = alignTL (vcat' with { _sep = 40 } (map (drawCM modelDetail entryNumberCutoff modelLayout emissiontype maxWidth) zippedInput))
    where zippedInput = zip4 cms alns comparisonNodeLabels colorList
          comparisonNodeLabels = map getBlankComparisonNodeLabels cms
          colorList = replicate (length cms) white

-- | Draw one or more CM
drawSingleCMs :: String -> Int -> String -> String -> Double -> [CM.CM] -> [(Maybe StockholmAlignment)] -> [QDiagram Cairo V2 Double Any]
drawSingleCMs modelDetail entryNumberCutoff modelLayout emissiontype maxWidth cms alns = map (drawCM modelDetail entryNumberCutoff modelLayout emissiontype maxWidth) zippedInput
    where zippedInput = zip4 cms alns comparisonNodeLabels colorList
          comparisonNodeLabels = map getBlankComparisonNodeLabels cms
          colorList = replicate (length cms) white

-- | Draw the guide Tree of a single CM
drawCM :: String -> Int -> String -> String -> Double -> (CM.CM,Maybe StockholmAlignment,V.Vector (Int,V.Vector (Colour Double)), Colour Double) -> QDiagram Cairo V2 Double Any
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
secondaryStructureVisualisation :: String -> Double -> [CM.CM] -> [(Maybe StockholmAlignment)] -> [CmcompareResult] -> [(String,String)]
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

buildFornaInput :: (CM.CM,Maybe StockholmAlignment,V.Vector (Int, V.Vector (Colour Double))) -> (String, String)
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

comparisonColLabelsToFornaLabel :: (Int, V.Vector (Colour Double)) -> String
comparisonColLabelsToFornaLabel (nodeNr,colorVector)
  | V.length colorVector > 1 =  " " ++ show nodeNr ++ ":red "
  | otherwise = ""

buildR2RInput :: (CM.CM, Maybe StockholmAlignment,V.Vector (Int,V.Vector (Colour Double))) -> (String,String)
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

comparisonColLabelsToR2RLabel :: (Int, V.Vector (Colour Double)) -> Char
comparisonColLabelsToR2RLabel (nodeNr,colorVector)
  | length colorVector > 1 = 's'
  | otherwise = '.'

nodeToColIndices :: (Int,(Int,V.Vector (Colour Double))) -> (Int,V.Vector (Colour Double))
nodeToColIndices (colIndex,(nodeIndex,colors)) = (colIndex,colors)

fillComparisonColLabels :: Int -> V.Vector (Int, V.Vector (Colour Double)) ->  V.Vector (Int, V.Vector (Colour Double))
fillComparisonColLabels maxEntryLength sparseComparisonColLabels = fullComparisonColLabels
   where fullComparisonColLabels = V.generate maxEntryLength (makeFullComparisonColLabel sparseComparisonColLabels)

makeFullComparisonColLabel :: V.Vector (Int, V.Vector (Colour Double)) -> Int -> (Int, V.Vector (Colour Double))
makeFullComparisonColLabel sparseComparisonColLabels colIndex = fullComparisonColLabel
  where availableLabel = V.find (\(a,c)-> colIndex == a) sparseComparisonColLabels
        fullComparisonColLabel = if isJust availableLabel then fromJust availableLabel else (colIndex,V.singleton white)

indexStructureToConnections :: (Int, Int, String, Int, Int) -> (String,String,Double,(Double,Double))
indexStructureToConnections (acc,emit,_,_,_) = (show emit,show acc,1,(0,0))

indexStructureRow :: (Int, Int, String, Int, Int) -> (Int, Int, String, Int, Int) -> Bool
indexStructureRow (row1,_,_,_,_)  (row2,_,_,_,_) = row1 == row2

indexStructureParent ::  (Int, Int, String, Int, Int) -> (Int, Int, String, Int, Int) -> Bool
indexStructureParent (_,p1,_,_,_)  (_,p2,_,_,_) = p1 == p2

data NodeIndices = S [Int] | L [Int] | R [Int]
  deriving (Show, Eq, Ord)

startState :: ([(Int,Int,String,Int,Int)],Int)
startState = ([],0::Int)

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
    _ -> put (interval,parentId)
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
    _ -> put (interval,intervalId)
  buildTreeIndexStructure nextId nodes xs

setNextId :: CM.NodeType -> Int -> Int -> Int
setNextId ntype intervalId newId
  | ntype == CM.NodeType 6 = newId
  | ntype == CM.NodeType 4 = newId
  | ntype == CM.NodeType 5 = newId
  | otherwise = intervalId

getIndexEnd :: V.Vector CM.Node -> [Int] -> Int
getIndexEnd nodes indices
  | null indices = (length nodes -1)
  | ntype == CM.NodeType 7 = currentIndex
  | ntype == CM.NodeType 0 = currentIndex
  | otherwise = getIndexEnd nodes remainingindices
   where currentIndex = head indices
         remainingindices = tail indices
         currentNode = nodes V.! currentIndex
         ntype = CM._ntype currentNode

--TODO swap with SVGFont
makeModelHeader :: String -> Colour Double -> QDiagram Cairo V2 Double Any
makeModelHeader mName modelColor = strutX 2 ||| setTitel mName ||| strutX 1 ||| rect 4 4 # lw 0.1 # fc modelColor
--setLabelLetter echar = alignedText 0.5 0.5 [echar] # fontSize 0.75 <> rect 0.4 0.5 # lw 0
--setStateLetter echar = alignedText 1 1 [echar] # fontSize 2.0 <> rect 2.0 2.0 # lw 0
--setTitelLetter echar = alignedText 0.5 0.5 [echar] # fontSize 4.0 <> rect 4.0 4.0 # lw 0
setLabel t = textSVG_ (TextOpts bitStreamFont INSIDE_H KERN False 0.75 0.75) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 0.75, negate 0.75))
setState t = textSVG_ (TextOpts bitStreamFont INSIDE_H KERN False 2 2) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 0.75, negate 0.75))
setTitel t = textSVG_ (TextOpts bitStreamFont INSIDE_H KERN False 4 4) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 0.75, negate 0.75))

drawCMNodeTree :: String -> [Char] -> String -> Int -> CM.States -> V.Vector (Int, V.Vector (Colour Double))-> V.Vector CM.Node -> [(Int, Int, String, Int, Int)] -> (Int,Int,String,Int,Int) -> QDiagram Cairo V2 Double Any
drawCMNodeTree modelDetail alphabetSymbols emissiontype boxlength allStates comparisonNodeLabels nodes indexStructure (intervalId,parentId,intervalType,currentIndex,currentEnd) = nodeTree
  where nodeTree = currentIntervalDrawing === hcat' with {_sep = 2} (map (drawCMNodeTree modelDetail alphabetSymbols emissiontype boxlength allStates comparisonNodeLabels nodes indexStructure) nextIntervals)
        nextIntervals = filter (\(_,p,_,_,_) -> intervalId == p) indexStructure
        currentIntervalDrawing = drawCMNodeInterval modelDetail alphabetSymbols emissiontype boxlength currentIndex currentEnd currentEnd allStates comparisonNodeLabels nodes (intervalId,parentId,intervalType,currentIndex,currentEnd) -- ||| (text' (show intervalId ++ "I" ++ show indexStructure) <> rect 100 100)

drawCMNodeRow :: String -> [Char] -> String -> Int -> Int -> Int -> Int -> CM.States -> V.Vector (Int, V.Vector (Colour Double))-> V.Vector CM.Node -> [(Int, Int, String, Int, Int)] -> QDiagram Cairo V2 Double Any
drawCMNodeRow modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes intervals = strutY 4 === hcat' with { _sep = 8 } (map (drawCMNodeInterval modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes) intervals)

drawCMNodeInterval :: String -> [Char] -> String -> Int -> Int -> Int -> Int -> CM.States -> V.Vector (Int, V.Vector (Colour Double))-> V.Vector CM.Node -> (Int, Int, String, Int, Int) -> QDiagram Cairo V2 Double Any
drawCMNodeInterval modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes (intervalId,parent,intervaltype,currentIndex,currentEnd)
  | modelDetail == "interval" = intervalVis
  | otherwise = nodeVis
  where intervalVis = rect 20 0 # named ("a" ++ intervalIdString)  # lw 0.0 === (rect 20 40 # lw 0.1 <> text' ((show currentIndex) ++ "-" ++ (show currentEnd))) === rect 20 0 # named ("e" ++ intervalIdString)  # lw 0.0 === strutY 5.0
        intervalIdString = show intervalId
        nodeVis = vcat' with { _sep = nodespacer } (map (drawCMNode modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes) currentInterval)
        currentInterval = [currentIndex..currentEnd]
        nodespacer = if (modelDetail == "detailed") then (2 :: Double) else (0 :: Double)

getCMNodeType :: CM.Node -> String
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

text' :: String -> QDiagram Cairo V2 Double Any
text' t = textSVG_ (TextOpts bitStreamFont INSIDE_H KERN False 3 3) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 0.75, negate 0.75))

textWithSize' :: String -> Double -> QDiagram Cairo V2 Double Any
textWithSize' t si = textSVG_ (TextOpts bitStreamFont INSIDE_H KERN False si si) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate siOffset, negate siOffset)) 
  where textLength = fromIntegral (length t) * 2
        siOffset = si/2

-- | Transform covariance model node labels to colors
labelToColor :: [Char] -> Colour Double
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

drawCMNode :: String -> [Char] -> String -> Int -> Int -> Int -> Int -> CM.States -> V.Vector (Int, V.Vector (Colour Double)) -> V.Vector CM.Node -> Int -> QDiagram Cairo V2 Double Any
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

colorBox :: Double -> Colour Double -> QDiagram Cairo V2 Double Any
colorBox singleBoxYLength colColour = rect 5 singleBoxYLength # fc colColour # lw 0.1
        
drawCMNodeBox :: [Char] -> String -> Int -> CM.States -> V.Vector (Int, V.Vector (Colour Double)) -> CM.Node -> QDiagram Cairo V2 Double Any
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

idBox :: String -> String -> [(Colour Double)] -> QDiagram Cairo V2 Double Any
idBox nId nType nodeLabels = (alignedText 0 0 nId # fontSize 2 # translate (r2 ((negate ((fromIntegral (length nId))/2)), negate 1.25)) <>  wheel nodeLabels # lw 0.1 <> rect 1.5 3 # lw 0) ||| strutX 2.0 ||| text' nType
nodeBox :: QDiagram Cairo V2 Double Any
nodeBox = rect 60 60 # lw 0.1

wheel :: [(Colour Double)] -> QDiagram Cairo V2 Double Any
wheel colors = wheel' # rotate r
   where
     wheel' = mconcat $ zipWith fc colors (iterateN n (rotate a) w)
     n = length colors
     a = 1 / (fromIntegral n) @@ turn
     w = wedge 3 xDir a # lwG 0
     r = (1/4 @@ turn)  ^-^  (1/(2*(fromIntegral n)) @@ turn)

--drawCMSplitStateBox :: Int -> [Char] -> String -> Double -> CM.States -> PI.PInt () CM.StateIndex -> QDiagram Cairo V2 Double Any
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
      Diagrams.Prelude.atop (position [((midpoint # translateX (negate 0.25 + xOffset) # translateY (0 + yOffset)), (setLabel (show (roundPos 2 weight))))])

makeSelfLabel (n1,n2,weight,(xOffset,yOffset))
  | weight == 0 = mempty
  | otherwise = withName ("e" ++ n1) $ \b1 ->
                withName ("e" ++ n2) $ \b2 ->
                  let v = location b2 .-. location b1
                      midpoint = location b1 .+^ (v ^/ 2)
                  in
                    Diagrams.Prelude.atop (position [(midpoint # translateX (negate 0.25 + xOffset) # translateY (0 + yOffset), setLabel (show (roundPos 2 weight)))])

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

bitStreamFontTuple = loadFont' "Bitstream" bitStreamString
bitStreamFont = snd bitStreamFontTuple

bitStreamString :: T.Text
bitStreamString = T.pack "<?xml version=\"1.0\" standalone=\"no\"?>\r\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" >\r\n<svg>\r\n<metadata>\r\nCreated by FontForge 20100429 at Thu Jul 14 09:20:15 2011\r\n By Tillmann Vogt,,,\r\nCopyright (c) 2003 by Bitstream, Inc. All Rights Reserved.\r\n</metadata>\r\n<defs>\r\n<font id=\"BitstreamVeraSansMono-Roman\" horiz-adv-x=\"1233\" >\r\n  <font-face \r\n    font-family=\"Bitstream Vera Sans Mono\"\r\n    font-weight=\"400\"\r\n    font-stretch=\"normal\"\r\n    units-per-em=\"2048\"\r\n    panose-1=\"2 11 6 9 3 8 4 2 2 4\"\r\n    ascent=\"1556\"\r\n    descent=\"-492\"\r\n    x-height=\"1118\"\r\n    cap-height=\"1493\"\r\n    bbox=\"-10 -483 1241 1901\"\r\n    underline-thickness=\"141\"\r\n    underline-position=\"-143\"\r\n    unicode-range=\"U+0020-FB02\"\r\n  />\r\n<missing-glyph \r\nd=\"M104 -362v1806h1024v-1806h-1024zM219 -248h795v1577h-795v-1577z\" />\r\n    <glyph glyph-name=\".notdef\" \r\nd=\"M104 -362v1806h1024v-1806h-1024zM219 -248h795v1577h-795v-1577z\" />\r\n    <glyph glyph-name=\".null\" horiz-adv-x=\"0\" \r\n />\r\n    <glyph glyph-name=\"nonmarkingreturn\" \r\n />\r\n    <glyph glyph-name=\"space\" unicode=\" \" \r\n />\r\n    <glyph glyph-name=\"exclam\" unicode=\"!\" \r\nd=\"M516 1493h203v-655l-21 -357h-161l-21 357v655zM516 254h203v-254h-203v254z\" />\r\n    <glyph glyph-name=\"quotedbl\" unicode=\"&#x22;\" \r\nd=\"M895 1493v-555h-174v555h174zM512 1493v-555h-174v555h174z\" />\r\n    <glyph glyph-name=\"numbersign\" unicode=\"#\" \r\nd=\"M684 1470l-104 -415h245l105 415h160l-105 -415h244v-154h-281l-84 -334h250v-153h-289l-104 -414h-160l105 414h-246l-105 -414h-159l104 414h-258v153h297l84 334h-266v154h303l104 415h160zM788 901h-245l-84 -334h246z\" />\r\n    <glyph glyph-name=\"dollar\" unicode=\"$\" \r\nd=\"M692 580v-434q110 3 172 61t62 158q0 93 -56 144.5t-178 70.5zM592 770v413q-104 -4 -162.5 -60t-58.5 -150q0 -86 54.5 -136t166.5 -67zM692 -301h-100l-1 301q-102 5 -202.5 28t-198.5 64v180q100 -62 201.5 -95t200.5 -35v458q-200 31 -301 122t-101 241\r\nq0 157 105.5 250.5t296.5 107.5v235h100l1 -235q79 -5 160 -20t165 -41v-173q-85 43 -165.5 66.5t-160.5 27.5v-431q206 -31 314 -128t108 -251t-116.5 -257t-304.5 -112z\" />\r\n    <glyph glyph-name=\"percent\" unicode=\"%\" \r\nd=\"M696 319q0 -78 52.5 -131t130.5 -53q77 0 130.5 53.5t53.5 130.5t-54 131t-130 54q-78 0 -130.5 -53t-52.5 -132zM561 319q0 135 92 227.5t226 92.5q64 0 121.5 -24t103.5 -70q46 -47 71 -105t25 -121q0 -133 -93 -226t-228 -93q-136 0 -227 91.5t-91 227.5zM121 465\r\nl-35 96l1042 418l41 -96zM168 1112q0 -79 52.5 -131.5t131.5 -52.5q77 0 131 53.5t54 130.5t-54 130.5t-131 53.5t-130.5 -53t-53.5 -131zM33 1112q0 135 92 227.5t227 92.5q64 0 122.5 -24t103.5 -69t69.5 -103.5t24.5 -123.5q0 -134 -93 -226.5t-227 -92.5q-135 0 -227 92\r\nt-92 227z\" />\r\n    <glyph glyph-name=\"ampersand\" unicode=\"&#x26;\" \r\nd=\"M547 907l416 -559q39 49 58 124t19 179q0 32 -3 102l-1 7h164v-39q0 -161 -37 -282.5t-112 -209.5l170 -229h-213l-78 109q-83 -70 -176 -104t-199 -34q-216 0 -357 134.5t-141 338.5q0 137 69 254t208 217q-50 72 -74 141t-24 140q0 150 99.5 237t272.5 87\r\nq65 0 130.5 -12t135.5 -35v-183q-59 39 -121.5 57.5t-131.5 18.5q-97 0 -153 -45.5t-56 -122.5q0 -59 29 -123.5t106 -167.5zM416 803q-92 -73 -137.5 -154.5t-45.5 -173.5q0 -151 100 -250.5t255 -99.5q42 0 88 12t90 35q27 15 44.5 26.5t33.5 24.5z\" />\r\n    <glyph glyph-name=\"quotesingle\" unicode=\"\'\" \r\nd=\"M702 1493v-555h-174v555h174z\" />\r\n    <glyph glyph-name=\"parenleft\" unicode=\"(\" \r\nd=\"M885 1554q-133 -228 -198.5 -454.5t-65.5 -456.5q0 -229 65.5 -456t198.5 -457h-160q-151 238 -225 463.5t-74 449.5q0 223 74 449t225 462h160z\" />\r\n    <glyph glyph-name=\"parenright\" unicode=\")\" \r\nd=\"M348 1554h160q151 -236 225 -462t74 -449q0 -225 -74 -451t-225 -462h-160q133 232 198.5 459t65.5 454q0 228 -65.5 455t-198.5 456z\" />\r\n    <glyph glyph-name=\"asterisk\" unicode=\"*\" \r\nd=\"M1067 1247l-358 -194l358 -195l-57 -98l-336 203v-377h-115v377l-336 -203l-57 98l358 195l-358 194l57 99l336 -203v377h115v-377l336 203z\" />\r\n    <glyph glyph-name=\"plus\" unicode=\"+\" \r\nd=\"M700 1171v-444h445v-170h-445v-444h-168v444h-444v170h444v444h168z\" />\r\n    <glyph glyph-name=\"comma\" unicode=\",\" \r\nd=\"M502 303h252v-207l-197 -383h-154l99 383v207z\" />\r\n    <glyph glyph-name=\"hyphen\" unicode=\"-\" \r\nd=\"M356 643h521v-164h-521v164z\" />\r\n    <glyph glyph-name=\"period\" unicode=\".\" \r\nd=\"M489 305h252v-305h-252v305z\" />\r\n    <glyph glyph-name=\"slash\" unicode=\"/\" \r\nd=\"M889 1493h190l-786 -1683h-191z\" />\r\n    <glyph glyph-name=\"zero\" unicode=\"0\" \r\nd=\"M483 750q0 55 38.5 95t92.5 40q56 0 96 -40t40 -95q0 -56 -39.5 -95t-96.5 -39q-56 0 -93.5 38t-37.5 96zM616 1360q-141 0 -210.5 -152t-69.5 -463q0 -310 69.5 -462t210.5 -152q142 0 211.5 152t69.5 462q0 311 -69.5 463t-211.5 152zM616 1520q239 0 361.5 -196\r\nt122.5 -579q0 -382 -122.5 -578t-361.5 -196t-361 196t-122 578q0 383 122 579t361 196z\" />\r\n    <glyph glyph-name=\"one\" unicode=\"1\" \r\nd=\"M270 170h314v1141l-338 -76v184l336 74h202v-1323h310v-170h-824v170z\" />\r\n    <glyph glyph-name=\"two\" unicode=\"2\" \r\nd=\"M373 170h686v-170h-907v170l327 348t193 213q100 122 135 197.5t35 154.5q0 125 -73.5 196t-201.5 71q-91 0 -191 -33t-212 -100v204q103 49 202.5 74t196.5 25q219 0 352.5 -116.5t133.5 -305.5q0 -96 -44.5 -192t-144.5 -212l-162.5 -180z\" />\r\n    <glyph glyph-name=\"three\" unicode=\"3\" \r\nd=\"M776 799q147 -39 225 -138.5t78 -248.5q0 -206 -138.5 -323.5t-383.5 -117.5q-103 0 -210 19t-210 55v201q102 -53 201 -79t197 -26q166 0 255 75t89 216q0 130 -89 206.5t-241 76.5h-154v166h154q139 0 217 61t78 170q0 115 -72.5 176.5t-206.5 61.5q-89 0 -184 -20\r\nt-199 -60v186q121 32 215.5 48t167.5 16q218 0 348.5 -109.5t130.5 -290.5q0 -123 -68.5 -205t-199.5 -116z\" />\r\n    <glyph glyph-name=\"four\" unicode=\"4\" \r\nd=\"M735 1309l-471 -789h471v789zM702 1493h234v-973h199v-164h-199v-356h-201v356h-633v191z\" />\r\n    <glyph glyph-name=\"five\" unicode=\"5\" \r\nd=\"M207 1493h756v-170h-572v-367q43 16 86.5 23.5t87.5 7.5q232 0 368 -137t136 -371q0 -236 -142.5 -372t-389.5 -136q-119 0 -217.5 16t-176.5 48v205q92 -50 185 -74.5t190 -24.5q167 0 257.5 88t90.5 250q0 160 -93.5 249t-260.5 89q-81 0 -158 -18.5t-147 -55.5v750z\r\n\" />\r\n    <glyph glyph-name=\"six\" unicode=\"6\" \r\nd=\"M991 1460v-186q-63 37 -134 56.5t-148 19.5q-192 0 -291 -144.5t-99 -425.5q48 100 133 153.5t195 53.5q216 0 334.5 -132.5t118.5 -375.5q0 -242 -122 -375t-343 -133q-260 0 -381 186.5t-121 587.5q0 378 145.5 576.5t421.5 198.5q74 0 148 -15.5t143 -44.5zM631 829\r\nq-129 0 -203 -93t-74 -257t74 -257t203 -93q134 0 202 88.5t68 261.5q0 174 -68 262t-202 88z\" />\r\n    <glyph glyph-name=\"seven\" unicode=\"7\" \r\nd=\"M139 1493h940v-86l-534 -1407h-211l520 1323h-715v170z\" />\r\n    <glyph glyph-name=\"eight\" unicode=\"8\" \r\nd=\"M616 709q-135 0 -208.5 -75.5t-73.5 -213.5t74.5 -214.5t207.5 -76.5q136 0 209.5 75.5t73.5 215.5q0 137 -74.5 213t-208.5 76zM440 793q-129 33 -201.5 123t-72.5 217q0 178 121 282.5t329 104.5q209 0 330 -104.5t121 -282.5q0 -127 -72.5 -217t-201.5 -123\r\nq150 -33 229.5 -133t79.5 -259q0 -202 -129 -316t-357 -114t-356.5 113.5t-128.5 314.5q0 160 79.5 260.5t229.5 133.5zM367 1114q0 -120 64 -183t185 -63q122 0 186 63t64 183q0 122 -63.5 186t-186.5 64q-121 0 -185 -64.5t-64 -185.5z\" />\r\n    <glyph glyph-name=\"nine\" unicode=\"9\" \r\nd=\"M596 662q129 0 202.5 93t73.5 257t-73.5 257t-202.5 93q-134 0 -202 -88.5t-68 -261.5q0 -174 67.5 -262t202.5 -88zM236 31v186q63 -37 134 -56.5t148 -19.5q192 0 290.5 144.5t98.5 425.5q-47 -100 -132 -153.5t-195 -53.5q-216 0 -334.5 133t-118.5 377\r\nq0 241 121.5 373.5t343.5 132.5q260 0 381 -187t121 -588q0 -377 -145.5 -575.5t-422.5 -198.5q-73 0 -147 15.5t-143 44.5z\" />\r\n    <glyph glyph-name=\"colon\" unicode=\":\" \r\nd=\"M489 1063h252v-303h-252v303zM489 305h252v-305h-252v305z\" />\r\n    <glyph glyph-name=\"semicolon\" unicode=\";\" \r\nd=\"M502 303h252v-207l-197 -383h-154l99 383v207zM489 1063h252v-303h-252v303z\" />\r\n    <glyph glyph-name=\"less\" unicode=\"&#x3c;\" \r\nd=\"M1145 961l-850 -320l850 -317v-183l-1057 418v166l1057 418v-182z\" />\r\n    <glyph glyph-name=\"equal\" unicode=\"=\" \r\nd=\"M88 524h1057v-172h-1057v172zM88 930h1057v-170h-1057v170z\" />\r\n    <glyph glyph-name=\"greater\" unicode=\"&#x3e;\" \r\nd=\"M88 961v182l1057 -418v-166l-1057 -418v183l850 317z\" />\r\n    <glyph glyph-name=\"question\" unicode=\"?\" \r\nd=\"M684 401h-190v154q0 98 30.5 166.5t114.5 150.5l90 89q62 59 85.5 103t23.5 93q0 89 -65.5 144t-174.5 55q-78 0 -167 -34.5t-187 -102.5v188q94 57 189.5 85t199.5 28q186 0 296.5 -96t110.5 -257q0 -76 -33.5 -141.5t-127.5 -157.5l-88 -86q-69 -66 -88 -108t-19 -103\r\nv-47v-123zM487 254h203v-254h-203v254z\" />\r\n    <glyph glyph-name=\"at\" unicode=\"@\" \r\nd=\"M1038 545q0 129 -64 206.5t-171 77.5t-171.5 -77.5t-64.5 -206.5q0 -130 64.5 -207.5t171.5 -77.5t171 77.5t64 207.5zM1178 135h-144v111q-37 -63 -102.5 -97t-147.5 -34q-161 0 -266.5 121t-105.5 309t105.5 309t266.5 121q80 0 147 -35t103 -96v63q0 156 -88 251\r\nt-233 95q-246 0 -391.5 -191.5t-145.5 -518.5q0 -329 165 -524t439 -195q54 0 108 10t111 31l48 -135q-63 -25 -124.5 -37t-119.5 -12q-357 0 -566.5 233t-209.5 629q0 390 188 621t504 231q209 0 334 -133t125 -357v-770z\" />\r\n    <glyph glyph-name=\"A\" unicode=\"A\" \r\nd=\"M616 1315l-213 -764h426zM494 1493h245l457 -1493h-209l-110 389h-523l-108 -389h-209z\" />\r\n    <glyph glyph-name=\"B\" unicode=\"B\" \r\nd=\"M369 713v-547h239q176 0 251 61.5t75 202.5q0 146 -79 214.5t-247 68.5h-239zM369 1327v-450h235q146 0 211.5 56t65.5 181q0 113 -64.5 163t-212.5 50h-235zM166 1493h442q229 0 353 -99t124 -280q0 -137 -65.5 -216t-196.5 -99q147 -22 230.5 -125.5t83.5 -263.5\r\nq0 -203 -133 -306.5t-396 -103.5h-442v1493z\" />\r\n    <glyph glyph-name=\"C\" unicode=\"C\" \r\nd=\"M1073 53q-77 -41 -158 -61.5t-172 -20.5q-287 0 -445.5 203t-158.5 571q0 366 159.5 570.5t444.5 204.5q91 0 172 -20.5t158 -61.5v-207q-74 61 -159 93t-171 32q-197 0 -295 -152t-98 -459q0 -306 98 -458t295 -152q88 0 172.5 32t157.5 93v-207z\" />\r\n    <glyph glyph-name=\"D\" unicode=\"D\" \r\nd=\"M436 166q255 0 356 125.5t101 453.5q0 331 -100.5 456.5t-356.5 125.5h-96v-1161h96zM440 1493q342 0 504 -182t162 -566q0 -382 -162 -563.5t-504 -181.5h-303v1493h303z\" />\r\n    <glyph glyph-name=\"E\" unicode=\"E\" \r\nd=\"M197 1493h886v-170h-684v-442h654v-170h-654v-541h703v-170h-905v1493z\" />\r\n    <glyph glyph-name=\"F\" unicode=\"F\" \r\nd=\"M233 1493h879v-170h-676v-440h613v-170h-613v-713h-203v1493z\" />\r\n    <glyph glyph-name=\"G\" unicode=\"G\" \r\nd=\"M1104 123q-81 -75 -182.5 -113.5t-219.5 -38.5q-284 0 -442 203.5t-158 570.5q0 366 160 570.5t445 204.5q94 0 180 -26.5t166 -80.5v-207q-81 77 -166 113.5t-180 36.5q-197 0 -295.5 -152.5t-98.5 -458.5q0 -311 95.5 -460.5t293.5 -149.5q67 0 117.5 15.5t91.5 48.5\r\nv401h-217v166h410v-643z\" />\r\n    <glyph glyph-name=\"H\" unicode=\"H\" \r\nd=\"M137 1493h203v-612h553v612h203v-1493h-203v711h-553v-711h-203v1493z\" />\r\n    <glyph glyph-name=\"I\" unicode=\"I\" \r\nd=\"M201 1493h829v-170h-313v-1153h313v-170h-829v170h313v1153h-313v170z\" />\r\n    <glyph glyph-name=\"J\" unicode=\"J\" \r\nd=\"M109 61v236q91 -81 188 -121.5t201 -40.5q143 0 199.5 74.5t56.5 277.5v836h-381v170h583v-1006q0 -282 -105.5 -399t-352.5 -117q-96 0 -191 22t-198 68z\" />\r\n    <glyph glyph-name=\"K\" unicode=\"K\" \r\nd=\"M137 1493h203v-664l631 664h237l-581 -610l598 -883h-244l-487 748l-154 -164v-584h-203v1493z\" />\r\n    <glyph glyph-name=\"L\" unicode=\"L\" \r\nd=\"M215 1493h203v-1323h721v-170h-924v1493z\" />\r\n    <glyph glyph-name=\"M\" unicode=\"M\" \r\nd=\"M86 1493h270l258 -760l260 760h271v-1493h-187v1319l-266 -787h-153l-267 787v-1319h-186v1493z\" />\r\n    <glyph glyph-name=\"N\" unicode=\"N\" \r\nd=\"M139 1493h256l504 -1229v1229h195v-1493h-256l-504 1229v-1229h-195v1493z\" />\r\n    <glyph glyph-name=\"O\" unicode=\"O\" \r\nd=\"M905 745q0 329 -67.5 470t-221.5 141q-153 0 -220.5 -141t-67.5 -470q0 -328 67.5 -469t220.5 -141q154 0 221.5 140.5t67.5 469.5zM1116 745q0 -390 -123.5 -582t-376.5 -192t-376 191t-123 583q0 391 123.5 583t375.5 192q253 0 376.5 -192t123.5 -583z\" />\r\n    <glyph glyph-name=\"P\" unicode=\"P\" \r\nd=\"M399 1327v-561h234q140 0 218.5 74t78.5 207t-78 206.5t-219 73.5h-234zM197 1493h436q250 0 379 -113.5t129 -332.5q0 -221 -128.5 -334t-379.5 -113h-234v-600h-202v1493z\" />\r\n    <glyph glyph-name=\"Q\" unicode=\"Q\" \r\nd=\"M655 -27q-7 0 -20 -1t-21 -1q-250 0 -373.5 192t-123.5 582q0 391 123.5 583t375.5 192q253 0 376.5 -192t123.5 -583q0 -294 -68.5 -473.5t-207.5 -251.5l200 -190l-151 -100zM905 745q0 329 -67.5 470t-221.5 141q-153 0 -220.5 -141t-67.5 -470q0 -328 67.5 -469\r\nt220.5 -141q154 0 221.5 140.5t67.5 469.5z\" />\r\n    <glyph glyph-name=\"R\" unicode=\"R\" \r\nd=\"M760 705q78 -20 133 -75.5t137 -221.5l203 -408h-217l-178 377q-77 161 -138.5 207.5t-160.5 46.5h-193v-631h-203v1493h416q246 0 377 -111t131 -321q0 -148 -80.5 -241.5t-226.5 -114.5zM346 1327v-530h221q145 0 216 65t71 199q0 129 -75.5 197.5t-219.5 68.5h-213z\r\n\" />\r\n    <glyph glyph-name=\"S\" unicode=\"S\" \r\nd=\"M1012 1442v-205q-92 59 -184.5 89t-186.5 30q-143 0 -226 -66.5t-83 -179.5q0 -99 54.5 -151t203.5 -87l106 -24q210 -49 306 -154t96 -286q0 -213 -132 -325t-384 -112q-105 0 -211 22.5t-213 67.5v215q115 -73 217.5 -107t206.5 -34q153 0 238 68.5t85 191.5\r\nq0 112 -58.5 171t-203.5 91l-108 25q-208 47 -302 142t-94 255q0 200 134.5 320.5t357.5 120.5q86 0 181 -19.5t200 -58.5z\" />\r\n    <glyph glyph-name=\"T\" unicode=\"T\" \r\nd=\"M47 1493h1139v-170h-467v-1323h-203v1323h-469v170z\" />\r\n    <glyph glyph-name=\"U\" unicode=\"U\" \r\nd=\"M147 573v920h203v-1012q0 -109 6 -155.5t21 -71.5q32 -59 92.5 -89t146.5 -30q87 0 147 30t93 89q15 25 21 71t6 154v94v920h202v-920q0 -229 -28.5 -325.5t-98.5 -159.5q-66 -59 -151 -88t-191 -29q-105 0 -190 29t-152 88q-69 62 -98 160.5t-29 324.5z\" />\r\n    <glyph glyph-name=\"V\" unicode=\"V\" \r\nd=\"M616 170l351 1323h209l-437 -1493h-245l-437 1493h209z\" />\r\n    <glyph glyph-name=\"W\" unicode=\"W\" \r\nd=\"M0 1493h197l143 -1212l170 802h211l172 -804l143 1214h197l-223 -1493h-191l-203 887l-202 -887h-191z\" />\r\n    <glyph glyph-name=\"X\" unicode=\"X\" \r\nd=\"M86 1493h217l328 -563l334 563h217l-447 -702l479 -791h-217l-366 643l-395 -643h-218l500 791z\" />\r\n    <glyph glyph-name=\"Y\" unicode=\"Y\" \r\nd=\"M37 1493h215l364 -659l363 659h217l-479 -823v-670h-203v670z\" />\r\n    <glyph glyph-name=\"Z\" unicode=\"Z\" \r\nd=\"M178 1493h969v-154l-780 -1169h802v-170h-1013v154l759 1169h-737v170z\" />\r\n    <glyph glyph-name=\"bracketleft\" unicode=\"[\" \r\nd=\"M463 1556h424v-143h-240v-1540h240v-143h-424v1826z\" />\r\n    <glyph glyph-name=\"backslash\" unicode=\"\\\" \r\nd=\"M293 1493l786 -1683h-190l-787 1683h191z\" />\r\n    <glyph glyph-name=\"bracketright\" unicode=\"]\" \r\nd=\"M770 1556v-1826h-424v143h240v1540h-240v143h424z\" />\r\n    <glyph glyph-name=\"asciicircum\" unicode=\"^\" \r\nd=\"M705 1493l456 -557h-178l-367 395l-366 -395h-178l456 557h177z\" />\r\n    <glyph glyph-name=\"underscore\" unicode=\"_\" \r\nd=\"M1024 -403v-80h-1024v80h1024z\" />\r\n    <glyph glyph-name=\"grave\" unicode=\"`\" \r\nd=\"M477 1638l281 -376h-154l-325 376h198z\" />\r\n    <glyph glyph-name=\"a\" unicode=\"a\" \r\nd=\"M702 563h-61q-161 0 -242.5 -56.5t-81.5 -168.5q0 -101 61 -157t169 -56q152 0 239 105.5t88 291.5v41h-172zM1059 639v-639h-185v166q-59 -100 -148.5 -147.5t-217.5 -47.5q-171 0 -273 96.5t-102 258.5q0 187 125.5 284t368.5 97h247v29q-1 134 -68 194.5t-214 60.5\r\nq-94 0 -190 -27t-187 -79v184q102 39 195.5 58.5t181.5 19.5q139 0 237.5 -41t159.5 -123q38 -50 54 -123.5t16 -220.5z\" />\r\n    <glyph glyph-name=\"b\" unicode=\"b\" \r\nd=\"M918 559q0 214 -68 323t-201 109q-134 0 -203 -109.5t-69 -322.5q0 -212 69 -322t203 -110q133 0 201 109t68 323zM377 977q44 82 121.5 126t179.5 44q202 0 318 -155.5t116 -428.5q0 -277 -116.5 -434.5t-319.5 -157.5q-100 0 -176.5 43.5t-122.5 126.5v-141h-184v1556\r\nh184v-579z\" />\r\n    <glyph glyph-name=\"c\" unicode=\"c\" \r\nd=\"M1061 57q-74 -43 -152.5 -64.5t-160.5 -21.5q-260 0 -406.5 156t-146.5 432t146.5 432t406.5 156q81 0 158 -21t155 -65v-193q-73 65 -146.5 94t-166.5 29q-173 0 -266 -112t-93 -320q0 -207 93.5 -319.5t265.5 -112.5q96 0 172 29.5t141 91.5v-191z\" />\r\n    <glyph glyph-name=\"d\" unicode=\"d\" \r\nd=\"M858 977v579h184v-1556h-184v141q-46 -83 -122.5 -126.5t-176.5 -43.5q-203 0 -319.5 157.5t-116.5 434.5q0 273 117 428.5t319 155.5q101 0 178 -43.5t121 -126.5zM317 559q0 -214 68 -323t201 -109t202.5 110t69.5 322q0 213 -69.5 322.5t-202.5 109.5t-201 -109\r\nt-68 -323z\" />\r\n    <glyph glyph-name=\"e\" unicode=\"e\" \r\nd=\"M1112 606v-90h-797v-6q0 -183 95.5 -283t269.5 -100q88 0 184 28t205 85v-183q-105 -43 -202.5 -64.5t-188.5 -21.5q-261 0 -408 156.5t-147 431.5q0 268 144 428t384 160q214 0 337.5 -145t123.5 -396zM928 660q-4 162 -76.5 246.5t-208.5 84.5q-133 0 -219 -88\r\nt-102 -244z\" />\r\n    <glyph glyph-name=\"f\" unicode=\"f\" \r\nd=\"M1063 1556v-153h-209q-99 0 -137.5 -40.5t-38.5 -143.5v-99h385v-143h-385v-977h-184v977h-299v143h299v78q0 184 84.5 271t263.5 87h221z\" />\r\n    <glyph glyph-name=\"g\" unicode=\"g\" \r\nd=\"M858 569q0 207 -67.5 314.5t-196.5 107.5q-135 0 -206 -107.5t-71 -314.5t71.5 -315.5t207.5 -108.5q127 0 194.5 109t67.5 315zM1042 72q0 -252 -119 -382t-350 -130q-76 0 -159 14t-166 41v182q98 -46 178 -68t147 -22q149 0 217 81t68 257v8v125q-44 -94 -120 -140\r\nt-185 -46q-196 0 -313 157t-117 420q0 264 117 421t313 157q108 0 183 -43t122 -133v145h184v-1044z\" />\r\n    <glyph glyph-name=\"h\" unicode=\"h\" \r\nd=\"M1051 694v-694h-185v694q0 151 -53 222t-166 71q-129 0 -198.5 -91.5t-69.5 -262.5v-633h-184v1556h184v-604q49 96 133 145.5t199 49.5q171 0 255.5 -112.5t84.5 -340.5z\" />\r\n    <glyph glyph-name=\"i\" unicode=\"i\" \r\nd=\"M256 1120h471v-977h365v-143h-914v143h365v834h-287v143zM543 1556h184v-233h-184v233z\" />\r\n    <glyph glyph-name=\"j\" unicode=\"j\" \r\nd=\"M600 -20v997h-317v143h501v-1140q0 -195 -89.5 -300.5t-254.5 -105.5h-254v156h234q90 0 135 62.5t45 187.5zM600 1556h184v-233h-184v233z\" />\r\n    <glyph glyph-name=\"k\" unicode=\"k\" \r\nd=\"M236 1556h190v-901l483 465h224l-441 -422l510 -698h-225l-414 578l-137 -129v-449h-190v1556z\" />\r\n    <glyph glyph-name=\"l\" unicode=\"l\" \r\nd=\"M639 406q0 -124 45.5 -187t134.5 -63h215v-156h-233q-165 0 -255.5 106t-90.5 300v1017h-295v144h479v-1161z\" />\r\n    <glyph glyph-name=\"m\" unicode=\"m\" \r\nd=\"M676 1006q34 72 86.5 106.5t126.5 34.5q135 0 190.5 -104.5t55.5 -393.5v-649h-168v641q0 237 -26.5 294.5t-96.5 57.5q-80 0 -109.5 -61.5t-29.5 -290.5v-641h-168v641q0 240 -28.5 296t-102.5 56q-73 0 -101.5 -61.5t-28.5 -290.5v-641h-167v1120h167v-96\r\nq33 60 82.5 91.5t112.5 31.5q76 0 126.5 -35t78.5 -106z\" />\r\n    <glyph glyph-name=\"n\" unicode=\"n\" \r\nd=\"M1051 694v-694h-185v694q0 151 -53 222t-166 71q-129 0 -198.5 -91.5t-69.5 -262.5v-633h-184v1120h184v-168q49 96 133 145.5t199 49.5q171 0 255.5 -112.5t84.5 -340.5z\" />\r\n    <glyph glyph-name=\"o\" unicode=\"o\" \r\nd=\"M616 991q-140 0 -212 -109t-72 -323q0 -213 72 -322.5t212 -109.5q141 0 213 109.5t72 322.5q0 214 -72 323t-213 109zM616 1147q233 0 356.5 -151t123.5 -437q0 -287 -123 -437.5t-357 -150.5q-233 0 -356 150.5t-123 437.5q0 286 123 437t356 151z\" />\r\n    <glyph glyph-name=\"p\" unicode=\"p\" \r\nd=\"M375 141v-567h-185v1546h185v-143q46 83 122.5 126.5t176.5 43.5q203 0 318.5 -157t115.5 -435q0 -273 -116 -428.5t-318 -155.5q-102 0 -178.5 43.5t-120.5 126.5zM915 559q0 214 -67.5 323t-200.5 109q-134 0 -203 -109.5t-69 -322.5q0 -212 69 -322t203 -110\r\nq133 0 200.5 109t67.5 323z\" />\r\n    <glyph glyph-name=\"q\" unicode=\"q\" \r\nd=\"M332 555q0 -214 67.5 -323t200.5 -109t201.5 109.5t68.5 322.5t-68.5 322.5t-201.5 109.5t-200.5 -109t-67.5 -323zM870 139q-45 -83 -121.5 -127.5t-177.5 -44.5q-201 0 -317.5 155.5t-116.5 428.5q0 278 116 435t318 157q100 0 176.5 -43.5t122.5 -126.5v143h185v-1546\r\nh-185v569z\" />\r\n    <glyph glyph-name=\"r\" unicode=\"r\" \r\nd=\"M1155 889q-59 46 -120 67t-134 21q-172 0 -263 -108t-91 -312v-557h-185v1120h185v-219q46 119 141.5 182.5t226.5 63.5q68 0 127 -17t113 -53v-188z\" />\r\n    <glyph glyph-name=\"s\" unicode=\"s\" \r\nd=\"M973 1081v-180q-79 46 -159 69t-163 23q-125 0 -186.5 -40.5t-61.5 -123.5q0 -75 46 -112t229 -72l74 -14q137 -26 207.5 -104t70.5 -203q0 -166 -118 -259.5t-328 -93.5q-83 0 -174 17.5t-197 52.5v190q103 -53 197 -79.5t178 -26.5q122 0 189 49.5t67 138.5\r\nq0 128 -245 177l-8 2l-69 14q-159 31 -232 104.5t-73 200.5q0 161 109 248.5t311 87.5q90 0 173 -16.5t163 -49.5z\" />\r\n    <glyph glyph-name=\"t\" unicode=\"t\" \r\nd=\"M614 1438v-318h418v-143h-418v-608q0 -124 47 -173t164 -49h207v-147h-225q-207 0 -292 83t-85 286v608h-299v143h299v318h184z\" />\r\n    <glyph glyph-name=\"u\" unicode=\"u\" \r\nd=\"M195 424v694h184v-694q0 -151 53.5 -222t165.5 -71q130 0 199 91.5t69 262.5v633h185v-1118h-185v168q-49 -97 -133.5 -147t-197.5 -50q-172 0 -256 112.5t-84 340.5z\" />\r\n    <glyph glyph-name=\"v\" unicode=\"v\" \r\nd=\"M100 1120h191l325 -940l326 940h191l-398 -1120h-237z\" />\r\n    <glyph glyph-name=\"w\" unicode=\"w\" \r\nd=\"M0 1120h182l195 -905l160 578h157l162 -578l195 905h182l-262 -1120h-176l-179 614l-178 -614h-176z\" />\r\n    <glyph glyph-name=\"x\" unicode=\"x\" \r\nd=\"M1118 1120l-401 -536l440 -584h-213l-328 449l-327 -449h-213l440 584l-401 536h204l297 -405l295 405h207z\" />\r\n    <glyph glyph-name=\"y\" unicode=\"y\" \r\nd=\"M858 360l-117 -308q-99 -264 -133 -322q-46 -78 -115 -117t-161 -39h-148v154h109q81 0 127 47t117 243l-433 1102h195l332 -876l327 876h195z\" />\r\n    <glyph glyph-name=\"z\" unicode=\"z\" \r\nd=\"M227 1122h813v-168l-643 -804h643v-150h-837v170l643 805h-619v147z\" />\r\n    <glyph glyph-name=\"braceleft\" unicode=\"{\" \r\nd=\"M1012 -190v-144h-64q-249 0 -333.5 74t-84.5 295v239q0 151 -53.5 209t-193.5 58h-62v143h62q141 0 194 57t53 207v240q0 221 84.5 294.5t333.5 73.5h64v-143h-70q-140 0 -182.5 -43.5t-42.5 -185.5v-248q0 -157 -45.5 -228t-155.5 -96q111 -27 156 -98t45 -227v-248\r\nq0 -143 42.5 -186t182.5 -43h70z\" />\r\n    <glyph glyph-name=\"bar\" unicode=\"|\" \r\nd=\"M702 1565v-2048h-172v2048h172z\" />\r\n    <glyph glyph-name=\"braceright\" unicode=\"}\" \r\nd=\"M221 -190h68q141 0 184 44t43 185v248q0 156 45 227t156 98q-110 25 -155.5 96t-45.5 228v248q0 141 -43 185t-184 44h-68v143h62q249 0 333 -73.5t84 -294.5v-240q0 -150 53.5 -207t194.5 -57h64v-143h-64q-141 0 -194.5 -58t-53.5 -209v-239q0 -221 -84 -295t-333 -74\r\nh-62v144z\" />\r\n    <glyph glyph-name=\"asciitilde\" unicode=\"~\" \r\nd=\"M1145 780v-174q-75 -59 -146.5 -86.5t-150.5 -27.5q-90 0 -203 51q-22 10 -33 14q-77 33 -128.5 45t-102.5 12q-79 0 -149.5 -29.5t-143.5 -92.5v174q78 60 151 87t156 27q53 0 103 -11t124 -42q12 -5 33 -15q116 -55 209 -55q70 0 138.5 30.5t142.5 92.5z\" />\r\n    <glyph glyph-name=\"Adieresis\" unicode=\"&#xc4;\" \r\nd=\"M616 1315l-213 -764h426zM494 1493h245l457 -1493h-209l-110 389h-523l-108 -389h-209zM319 1870h203v-203h-203v203zM711 1870h202v-203h-202v203z\" />\r\n    <glyph glyph-name=\"Aring\" unicode=\"&#xc5;\" \r\nd=\"M768 1626q0 63 -44.5 107.5t-107.5 44.5q-64 0 -107.5 -43.5t-43.5 -108.5q0 -63 44 -107t107 -44t107.5 44t44.5 107zM616 1311l-213 -760h426zM465 1399q-58 33 -90.5 93.5t-32.5 133.5q0 114 80 194.5t194 80.5t194.5 -80.5t80.5 -194.5q0 -73 -32 -132t-91 -95\r\nl428 -1399h-209l-110 389h-523l-108 -389h-209z\" />\r\n    <glyph glyph-name=\"Ccedilla\" unicode=\"&#xc7;\" \r\nd=\"M1073 53q-77 -41 -158 -61.5t-172 -20.5q-287 0 -445.5 203t-158.5 571q0 366 159.5 570.5t444.5 204.5q91 0 172 -20.5t158 -61.5v-207q-74 61 -159 93t-171 32q-197 0 -295 -152t-98 -459q0 -306 98 -458t295 -152q88 0 172.5 32t157.5 93v-207zM800 0\r\nq56 -62 82.5 -114.5t26.5 -100.5q0 -89 -60 -134.5t-178 -45.5q-45 0 -88.5 6t-87.5 18v131q34 -17 71.5 -24.5t84.5 -7.5q58 0 88.5 24t30.5 70q0 30 -22 73.5t-66 104.5h118z\" />\r\n    <glyph glyph-name=\"Eacute\" unicode=\"&#xc9;\" \r\nd=\"M197 1493h886v-170h-684v-442h654v-170h-654v-541h703v-170h-905v1493zM690 1899h186l-229 -264h-154z\" />\r\n    <glyph glyph-name=\"Ntilde\" unicode=\"&#xd1;\" \r\nd=\"M139 1493h256l504 -1229v1229h195v-1493h-256l-504 1229v-1229h-195v1493zM594 1710l-57 33q-25 14 -40.5 19.5t-27.5 5.5q-35 0 -55 -25t-20 -70v-6h-125q0 101 51.5 160t136.5 59q36 0 66.5 -8t79.5 -35l57 -30q22 -13 39.5 -19t32.5 -6q31 0 51 25.5t20 66.5v6h125\r\nq-2 -100 -53 -159.5t-135 -59.5q-34 0 -64 8t-82 35z\" />\r\n    <glyph glyph-name=\"Odieresis\" unicode=\"&#xd6;\" \r\nd=\"M905 745q0 329 -67.5 470t-221.5 141q-153 0 -220.5 -141t-67.5 -470q0 -328 67.5 -469t220.5 -141q154 0 221.5 140.5t67.5 469.5zM1116 745q0 -390 -123.5 -582t-376.5 -192t-376 191t-123 583q0 391 123.5 583t375.5 192q253 0 376.5 -192t123.5 -583zM319 1870h203\r\nv-203h-203v203zM711 1870h202v-203h-202v203z\" />\r\n    <glyph glyph-name=\"Udieresis\" unicode=\"&#xdc;\" \r\nd=\"M147 573v920h203v-1012q0 -109 6 -155.5t21 -71.5q32 -59 92.5 -89t146.5 -30q87 0 147 30t93 89q15 25 21 71t6 154v94v920h202v-920q0 -229 -28.5 -325.5t-98.5 -159.5q-66 -59 -151 -88t-191 -29q-105 0 -190 29t-152 88q-69 62 -98 160.5t-29 324.5zM319 1870h203\r\nv-203h-203v203zM711 1870h202v-203h-202v203z\" />\r\n    <glyph glyph-name=\"aacute\" unicode=\"&#xe1;\" \r\nd=\"M702 563h-61q-161 0 -242.5 -56.5t-81.5 -168.5q0 -101 61 -157t169 -56q152 0 239 105.5t88 291.5v41h-172zM1059 639v-639h-185v166q-59 -100 -148.5 -147.5t-217.5 -47.5q-171 0 -273 96.5t-102 258.5q0 187 125.5 284t368.5 97h247v29q-1 134 -68 194.5t-214 60.5\r\nq-94 0 -190 -27t-187 -79v184q102 39 195.5 58.5t181.5 19.5q139 0 237.5 -41t159.5 -123q38 -50 54 -123.5t16 -220.5zM756 1638h198l-325 -376h-154z\" />\r\n    <glyph glyph-name=\"agrave\" unicode=\"&#xe0;\" \r\nd=\"M702 563h-61q-161 0 -242.5 -56.5t-81.5 -168.5q0 -101 61 -157t169 -56q152 0 239 105.5t88 291.5v41h-172zM1059 639v-639h-185v166q-59 -100 -148.5 -147.5t-217.5 -47.5q-171 0 -273 96.5t-102 258.5q0 187 125.5 284t368.5 97h247v29q-1 134 -68 194.5t-214 60.5\r\nq-94 0 -190 -27t-187 -79v184q102 39 195.5 58.5t181.5 19.5q139 0 237.5 -41t159.5 -123q38 -50 54 -123.5t16 -220.5zM477 1638l281 -376h-154l-325 376h198z\" />\r\n    <glyph glyph-name=\"acircumflex\" unicode=\"&#xe2;\" \r\nd=\"M702 563h-61q-161 0 -242.5 -56.5t-81.5 -168.5q0 -101 61 -157t169 -56q152 0 239 105.5t88 291.5v41h-172zM1059 639v-639h-185v166q-59 -100 -148.5 -147.5t-217.5 -47.5q-171 0 -273 96.5t-102 258.5q0 187 125.5 284t368.5 97h247v29q-1 134 -68 194.5t-214 60.5\r\nq-94 0 -190 -27t-187 -79v184q102 39 195.5 58.5t181.5 19.5q139 0 237.5 -41t159.5 -123q38 -50 54 -123.5t16 -220.5zM543 1638h147l246 -376h-139l-181 245l-180 -245h-139z\" />\r\n    <glyph glyph-name=\"adieresis\" unicode=\"&#xe4;\" \r\nd=\"M702 563h-61q-161 0 -242.5 -56.5t-81.5 -168.5q0 -101 61 -157t169 -56q152 0 239 105.5t88 291.5v41h-172zM1059 639v-639h-185v166q-59 -100 -148.5 -147.5t-217.5 -47.5q-171 0 -273 96.5t-102 258.5q0 187 125.5 284t368.5 97h247v29q-1 134 -68 194.5t-214 60.5\r\nq-94 0 -190 -27t-187 -79v184q102 39 195.5 58.5t181.5 19.5q139 0 237.5 -41t159.5 -123q38 -50 54 -123.5t16 -220.5zM319 1552h203v-202h-203v202zM711 1552h202v-202h-202v202z\" />\r\n    <glyph glyph-name=\"atilde\" unicode=\"&#xe3;\" \r\nd=\"M702 563h-61q-161 0 -242.5 -56.5t-81.5 -168.5q0 -101 61 -157t169 -56q152 0 239 105.5t88 291.5v41h-172zM1059 639v-639h-185v166q-59 -100 -148.5 -147.5t-217.5 -47.5q-171 0 -273 96.5t-102 258.5q0 187 125.5 284t368.5 97h247v29q-1 134 -68 194.5t-214 60.5\r\nq-94 0 -190 -27t-187 -79v184q102 39 195.5 58.5t181.5 19.5q139 0 237.5 -41t159.5 -123q38 -50 54 -123.5t16 -220.5zM612 1370l-57 55q-21 20 -37.5 29.5t-30.5 9.5q-38 0 -56 -37t-20 -118h-124q1 134 52 208t142 74q39 0 71 -14t69 -47l57 -55q21 -20 37.5 -29.5\r\nt29.5 -9.5q39 0 57 37t19 118h125q-1 -134 -52 -208t-142 -74q-39 0 -71 14t-69 47z\" />\r\n    <glyph glyph-name=\"aring\" unicode=\"&#xe5;\" \r\nd=\"M702 563h-61q-161 0 -242.5 -56.5t-81.5 -168.5q0 -101 61 -157t169 -56q152 0 239 105.5t88 291.5v41h-172zM1059 639v-639h-185v166q-59 -100 -148.5 -147.5t-217.5 -47.5q-171 0 -273 96.5t-102 258.5q0 187 125.5 284t368.5 97h247v29q-1 134 -68 194.5t-214 60.5\r\nq-94 0 -190 -27t-187 -79v184q102 39 195.5 58.5t181.5 19.5q139 0 237.5 -41t159.5 -123q38 -50 54 -123.5t16 -220.5zM891 1524q0 -115 -79.5 -195t-195.5 -80q-115 0 -194.5 80t-79.5 195t79.5 194.5t194.5 79.5q116 0 195.5 -79.5t79.5 -194.5zM768 1524q0 63 -44 107\r\nt-108 44t-107.5 -43.5t-43.5 -107.5q0 -65 43.5 -108.5t107.5 -43.5t108 44t44 108z\" />\r\n    <glyph glyph-name=\"ccedilla\" unicode=\"&#xe7;\" \r\nd=\"M1061 57q-74 -43 -152.5 -64.5t-160.5 -21.5q-260 0 -406.5 156t-146.5 432t146.5 432t406.5 156q81 0 158 -21t155 -65v-193q-73 65 -146.5 94t-166.5 29q-173 0 -266 -112t-93 -320q0 -207 93.5 -319.5t265.5 -112.5q96 0 172 29.5t141 91.5v-191zM804 0\r\nq56 -62 82.5 -114.5t26.5 -100.5q0 -89 -60 -134.5t-178 -45.5q-45 0 -88.5 6t-87.5 18v131q34 -17 71.5 -24.5t84.5 -7.5q58 0 88.5 24t30.5 70q0 30 -22 73.5t-66 104.5h118z\" />\r\n    <glyph glyph-name=\"eacute\" unicode=\"&#xe9;\" \r\nd=\"M1112 606v-90h-797v-6q0 -183 95.5 -283t269.5 -100q88 0 184 28t205 85v-183q-105 -43 -202.5 -64.5t-188.5 -21.5q-261 0 -408 156.5t-147 431.5q0 268 144 428t384 160q214 0 337.5 -145t123.5 -396zM928 660q-4 162 -76.5 246.5t-208.5 84.5q-133 0 -219 -88\r\nt-102 -244zM770 1638h198l-325 -376h-154z\" />\r\n    <glyph glyph-name=\"egrave\" unicode=\"&#xe8;\" \r\nd=\"M1112 606v-90h-797v-6q0 -183 95.5 -283t269.5 -100q88 0 184 28t205 85v-183q-105 -43 -202.5 -64.5t-188.5 -21.5q-261 0 -408 156.5t-147 431.5q0 268 144 428t384 160q214 0 337.5 -145t123.5 -396zM928 660q-4 162 -76.5 246.5t-208.5 84.5q-133 0 -219 -88\r\nt-102 -244zM491 1638l281 -376h-154l-325 376h198z\" />\r\n    <glyph glyph-name=\"ecircumflex\" unicode=\"&#xea;\" \r\nd=\"M1112 606v-90h-797v-6q0 -183 95.5 -283t269.5 -100q88 0 184 28t205 85v-183q-105 -43 -202.5 -64.5t-188.5 -21.5q-261 0 -408 156.5t-147 431.5q0 268 144 428t384 160q214 0 337.5 -145t123.5 -396zM928 660q-4 162 -76.5 246.5t-208.5 84.5q-133 0 -219 -88\r\nt-102 -244zM557 1638h147l246 -376h-139l-181 245l-180 -245h-139z\" />\r\n    <glyph glyph-name=\"edieresis\" unicode=\"&#xeb;\" \r\nd=\"M1112 606v-90h-797v-6q0 -183 95.5 -283t269.5 -100q88 0 184 28t205 85v-183q-105 -43 -202.5 -64.5t-188.5 -21.5q-261 0 -408 156.5t-147 431.5q0 268 144 428t384 160q214 0 337.5 -145t123.5 -396zM928 660q-4 162 -76.5 246.5t-208.5 84.5q-133 0 -219 -88\r\nt-102 -244zM333 1552h203v-202h-203v202zM725 1552h202v-202h-202v202z\" />\r\n    <glyph glyph-name=\"iacute\" unicode=\"&#xed;\" \r\nd=\"M256 1120h471v-977h365v-143h-914v143h365v834h-287v143zM756 1638h198l-325 -376h-154z\" />\r\n    <glyph glyph-name=\"igrave\" unicode=\"&#xec;\" \r\nd=\"M256 1120h471v-977h365v-143h-914v143h365v834h-287v143zM477 1638l281 -376h-154l-325 376h198z\" />\r\n    <glyph glyph-name=\"icircumflex\" unicode=\"&#xee;\" \r\nd=\"M256 1120h471v-977h365v-143h-914v143h365v834h-287v143zM543 1638h147l246 -376h-139l-181 245l-180 -245h-139z\" />\r\n    <glyph glyph-name=\"idieresis\" unicode=\"&#xef;\" \r\nd=\"M256 1120h471v-977h365v-143h-914v143h365v834h-287v143zM319 1552h203v-202h-203v202zM711 1552h202v-202h-202v202z\" />\r\n    <glyph glyph-name=\"ntilde\" unicode=\"&#xf1;\" \r\nd=\"M1051 694v-694h-185v694q0 151 -53 222t-166 71q-129 0 -198.5 -91.5t-69.5 -262.5v-633h-184v1120h184v-168q49 96 133 145.5t199 49.5q171 0 255.5 -112.5t84.5 -340.5zM612 1370l-57 55q-21 20 -37.5 29.5t-30.5 9.5q-38 0 -56 -37t-20 -118h-124q1 134 52 208t142 74\r\nq39 0 71 -14t69 -47l57 -55q21 -20 37.5 -29.5t29.5 -9.5q39 0 57 37t19 118h125q-1 -134 -52 -208t-142 -74q-39 0 -71 14t-69 47z\" />\r\n    <glyph glyph-name=\"oacute\" unicode=\"&#xf3;\" \r\nd=\"M616 991q-140 0 -212 -109t-72 -323q0 -213 72 -322.5t212 -109.5q141 0 213 109.5t72 322.5q0 214 -72 323t-213 109zM616 1147q233 0 356.5 -151t123.5 -437q0 -287 -123 -437.5t-357 -150.5q-233 0 -356 150.5t-123 437.5q0 286 123 437t356 151zM756 1638h198\r\nl-325 -376h-154z\" />\r\n    <glyph glyph-name=\"ograve\" unicode=\"&#xf2;\" \r\nd=\"M616 991q-140 0 -212 -109t-72 -323q0 -213 72 -322.5t212 -109.5q141 0 213 109.5t72 322.5q0 214 -72 323t-213 109zM616 1147q233 0 356.5 -151t123.5 -437q0 -287 -123 -437.5t-357 -150.5q-233 0 -356 150.5t-123 437.5q0 286 123 437t356 151zM477 1638l281 -376\r\nh-154l-325 376h198z\" />\r\n    <glyph glyph-name=\"ocircumflex\" unicode=\"&#xf4;\" \r\nd=\"M616 991q-140 0 -212 -109t-72 -323q0 -213 72 -322.5t212 -109.5q141 0 213 109.5t72 322.5q0 214 -72 323t-213 109zM616 1147q233 0 356.5 -151t123.5 -437q0 -287 -123 -437.5t-357 -150.5q-233 0 -356 150.5t-123 437.5q0 286 123 437t356 151zM543 1638h147\r\nl246 -376h-139l-181 245l-180 -245h-139z\" />\r\n    <glyph glyph-name=\"odieresis\" unicode=\"&#xf6;\" \r\nd=\"M616 991q-140 0 -212 -109t-72 -323q0 -213 72 -322.5t212 -109.5q141 0 213 109.5t72 322.5q0 214 -72 323t-213 109zM616 1147q233 0 356.5 -151t123.5 -437q0 -287 -123 -437.5t-357 -150.5q-233 0 -356 150.5t-123 437.5q0 286 123 437t356 151zM319 1552h203v-202\r\nh-203v202zM711 1552h202v-202h-202v202z\" />\r\n    <glyph glyph-name=\"otilde\" unicode=\"&#xf5;\" \r\nd=\"M616 991q-140 0 -212 -109t-72 -323q0 -213 72 -322.5t212 -109.5q141 0 213 109.5t72 322.5q0 214 -72 323t-213 109zM616 1147q233 0 356.5 -151t123.5 -437q0 -287 -123 -437.5t-357 -150.5q-233 0 -356 150.5t-123 437.5q0 286 123 437t356 151zM612 1370l-57 55\r\nq-21 20 -37.5 29.5t-30.5 9.5q-38 0 -56 -37t-20 -118h-124q1 134 52 208t142 74q39 0 71 -14t69 -47l57 -55q21 -20 37.5 -29.5t29.5 -9.5q39 0 57 37t19 118h125q-1 -134 -52 -208t-142 -74q-39 0 -71 14t-69 47z\" />\r\n    <glyph glyph-name=\"uacute\" unicode=\"&#xfa;\" \r\nd=\"M195 424v694h184v-694q0 -151 53.5 -222t165.5 -71q130 0 199 91.5t69 262.5v633h185v-1118h-185v168q-49 -97 -133.5 -147t-197.5 -50q-172 0 -256 112.5t-84 340.5zM756 1638h198l-325 -376h-154z\" />\r\n    <glyph glyph-name=\"ugrave\" unicode=\"&#xf9;\" \r\nd=\"M195 424v694h184v-694q0 -151 53.5 -222t165.5 -71q130 0 199 91.5t69 262.5v633h185v-1118h-185v168q-49 -97 -133.5 -147t-197.5 -50q-172 0 -256 112.5t-84 340.5zM477 1638l281 -376h-154l-325 376h198z\" />\r\n    <glyph glyph-name=\"ucircumflex\" unicode=\"&#xfb;\" \r\nd=\"M195 424v694h184v-694q0 -151 53.5 -222t165.5 -71q130 0 199 91.5t69 262.5v633h185v-1118h-185v168q-49 -97 -133.5 -147t-197.5 -50q-172 0 -256 112.5t-84 340.5zM543 1638h147l246 -376h-139l-181 245l-180 -245h-139z\" />\r\n    <glyph glyph-name=\"udieresis\" unicode=\"&#xfc;\" \r\nd=\"M195 424v694h184v-694q0 -151 53.5 -222t165.5 -71q130 0 199 91.5t69 262.5v633h185v-1118h-185v168q-49 -97 -133.5 -147t-197.5 -50q-172 0 -256 112.5t-84 340.5zM319 1552h203v-202h-203v202zM711 1552h202v-202h-202v202z\" />\r\n    <glyph glyph-name=\"dagger\" unicode=\"&#x2020;\" \r\nd=\"M528 1493h177v-420h366v-153h-366v-1117h-177v1117h-366v153h366v420z\" />\r\n    <glyph glyph-name=\"degree\" unicode=\"&#xb0;\" \r\nd=\"M616 1520q65 0 123.5 -24.5t102.5 -70.5q45 -45 68.5 -102t23.5 -123q0 -132 -93 -223.5t-227 -91.5q-135 0 -225 90t-90 225q0 134 92 227t225 93zM616 1391q-79 0 -134.5 -55.5t-55.5 -135.5t54.5 -134t133.5 -54q80 0 136.5 55t56.5 133q0 79 -56 135t-135 56z\" />\r\n    <glyph glyph-name=\"cent\" unicode=\"&#xa2;\" \r\nd=\"M1061 1077v-172q-67 40 -132 62t-128 26v-870q63 5 128.5 27.5t131.5 62.5v-172q-73 -31 -138 -48t-122 -20v-286h-103v286q-225 22 -355 178.5t-130 407.5q0 250 131.5 408.5t353.5 177.5v287h103v-287q57 -3 122 -20t138 -48zM698 127v864q-132 -12 -212 -130t-80 -302\r\nt80 -301.5t212 -130.5z\" />\r\n    <glyph glyph-name=\"sterling\" unicode=\"&#xa3;\" \r\nd=\"M1092 1462v-184q-62 44 -125.5 66t-130.5 22q-134 0 -197.5 -89.5t-63.5 -281.5v-217h371v-143h-371v-465h537v-170h-973v170h236v465h-199v143h199v238q0 254 109.5 379t332.5 125q65 0 133.5 -14.5t141.5 -43.5z\" />\r\n    <glyph glyph-name=\"section\" unicode=\"&#xa7;\" \r\nd=\"M936 1462v-164q-83 39 -154.5 58.5t-128.5 19.5q-97 0 -150.5 -40t-53.5 -111q0 -90 205 -205l40 -23q212 -119 277 -196t65 -178q0 -90 -45.5 -160t-138.5 -123q61 -52 90 -106.5t29 -118.5q0 -142 -102 -226t-275 -84q-73 0 -150.5 14.5t-164.5 43.5v164\r\nq87 -39 161 -58.5t131 -19.5q102 0 158.5 42t56.5 118q0 102 -221 225l-24 14q-214 120 -278 196.5t-64 178.5q0 91 46.5 162.5t137.5 120.5q-60 44 -89.5 100t-29.5 125q0 130 100 209.5t267 79.5q72 0 148.5 -14.5t156.5 -43.5zM485 936q-62 -45 -92.5 -90t-30.5 -92\r\nq0 -76 70.5 -143.5t317.5 -200.5q62 45 92 90t30 92q0 76 -71.5 144t-315.5 200z\" />\r\n    <glyph glyph-name=\"bullet\" unicode=\"&#x2022;\" \r\nd=\"M319 762q0 124 86 209.5t211 85.5q124 0 210.5 -86t86.5 -209q0 -124 -87 -210.5t-212 -86.5q-124 0 -209.5 85.5t-85.5 211.5z\" />\r\n    <glyph glyph-name=\"paragraph\" unicode=\"&#xb6;\" \r\nd=\"M582 1493h448v-1690h-141v1567h-191v-1567h-141v846q-215 17 -333 127.5t-118 294.5q0 190 130.5 306t345.5 116z\" />\r\n    <glyph glyph-name=\"germandbls\" unicode=\"&#xdf;\" \r\nd=\"M188 1137q0 213 105 316t321 103q204 0 309 -110.5t107 -326.5q-155 -14 -239 -76t-84 -162q0 -49 27.5 -87.5t94.5 -80.5l58 -37q151 -93 206.5 -175t55.5 -198q0 -154 -112 -243t-308 -89q-69 0 -136.5 12.5t-133.5 36.5v164q76 -31 142.5 -46t125.5 -15\r\nq108 0 172 48.5t64 129.5q0 71 -32.5 118.5t-152.5 121.5l-67 39q-92 56 -137.5 122.5t-45.5 145.5q0 128 81 213.5t237 120.5q-2 107 -62.5 164t-173.5 57q-121 0 -178 -65.5t-57 -204.5v-1133h-187v1137z\" />\r\n    <glyph glyph-name=\"registered\" unicode=\"&#xae;\" \r\nd=\"M600 1100q147 0 219 -49t72 -150q0 -71 -44 -117.5t-124 -60.5q19 -5 48 -38.5t67 -92.5l114 -186h-143l-107 174q-49 80 -79.5 101t-75.5 21h-55v-296h-130v694h238zM594 1016h-102v-232h102q90 0 127 27t37 90q0 62 -37 88.5t-127 26.5zM616 1358q127 0 236 -45\r\nt199 -135q90 -91 136 -201t46 -236q0 -125 -45.5 -234t-136.5 -200t-200 -136.5t-235 -45.5q-125 0 -234 45.5t-200 136.5t-136.5 200t-45.5 234q0 126 46 236t136 201q90 90 199 135t235 45zM616 1255q-106 0 -196.5 -37t-165.5 -112t-113.5 -167t-38.5 -198\r\nq0 -104 38.5 -195.5t113.5 -166.5q76 -76 166.5 -114t195.5 -38q106 0 196.5 38t166.5 114t113.5 166.5t37.5 195.5q0 106 -38 198t-113 167t-165.5 112t-197.5 37z\" />\r\n    <glyph glyph-name=\"copyright\" unicode=\"&#xa9;\" \r\nd=\"M864 1071v-108q-58 28 -113.5 41.5t-113.5 13.5q-117 0 -184.5 -74.5t-67.5 -202.5q0 -132 70 -203t200 -71q56 0 107.5 12.5t101.5 38.5v-104q-52 -23 -109 -34t-118 -11q-180 0 -283.5 100t-103.5 272q0 173 104 274t283 101q61 0 118 -11t109 -34zM616 1255\r\nq-106 0 -196.5 -37t-165.5 -112t-113.5 -167t-38.5 -198q0 -104 38.5 -195.5t113.5 -166.5q76 -76 166.5 -114t195.5 -38q106 0 196.5 38t166.5 114t113.5 166.5t37.5 195.5q0 106 -38 198t-113 167t-165.5 112t-197.5 37zM616 1358q127 0 236 -45t199 -135q90 -91 136 -201\r\nt46 -236q0 -125 -45.5 -234t-136.5 -200t-200 -136.5t-235 -45.5q-125 0 -234 45.5t-200 136.5t-136.5 200t-45.5 234q0 126 46 236t136 201q90 90 199 135t235 45z\" />\r\n    <glyph glyph-name=\"trademark\" unicode=\"&#x2122;\" \r\nd=\"M438 1493v-94h-162v-484h-114v484h-162v94h438zM692 1493l137 -256l125 256h172v-578h-114v482l-156 -301h-55l-166 301v-482h-113v578h170z\" />\r\n    <glyph glyph-name=\"acute\" unicode=\"&#xb4;\" \r\nd=\"M756 1638h198l-325 -376h-154z\" />\r\n    <glyph glyph-name=\"dieresis\" unicode=\"&#xa8;\" \r\nd=\"M319 1552h203v-202h-203v202zM711 1552h202v-202h-202v202z\" />\r\n    <glyph glyph-name=\"notequal\" unicode=\"&#x2260;\" \r\nd=\"M88 930h647l246 315l125 -102l-164 -213h203v-170h-334l-184 -236h518v-172h-647l-248 -315l-125 104l164 211h-201v172h330l184 236h-514v170z\" />\r\n    <glyph glyph-name=\"AE\" unicode=\"&#xc6;\" \r\nd=\"M1161 1493v-170h-338v-442h307v-170h-307v-541h357v-170h-543v383h-352l-101 -383h-184l410 1493h751zM530 1323l-202 -772h309v772h-107z\" />\r\n    <glyph glyph-name=\"Oslash\" unicode=\"&#xd8;\" \r\nd=\"M371 303q30 -78 95.5 -123t149.5 -45q154 0 221.5 140.5t67.5 469.5q0 110 -5 178t-15 113zM338 471l504 719q-25 81 -82.5 123.5t-143.5 42.5q-157 0 -222.5 -146t-65.5 -524q0 -80 2.5 -130.5t7.5 -84.5zM1032 1247q41 -81 62.5 -210t21.5 -292q0 -390 -123.5 -582\r\nt-376.5 -192q-121 0 -211 40.5t-151 121.5l-143 -203l-103 70l178 252q-32 73 -50.5 204t-18.5 289q0 391 123.5 583t375.5 192q115 0 201.5 -41t143.5 -121l139 201l100 -74z\" />\r\n    <glyph glyph-name=\"infinity\" unicode=\"&#x221e;\" \r\nd=\"M694 627q57 -136 102.5 -186t106.5 -50q71 0 116.5 67t45.5 175q0 107 -42 171t-112 64q-66 0 -118.5 -58.5t-98.5 -182.5zM537 635q-56 134 -102 183.5t-107 49.5q-71 0 -116.5 -66.5t-45.5 -172.5q0 -109 41.5 -173.5t109.5 -64.5q67 0 120 59t100 185zM616 764\r\nq47 117 120.5 180.5t164.5 63.5q119 0 205 -109.5t86 -269.5q0 -167 -81.5 -273t-207.5 -106q-83 0 -147 50t-142 181q-65 -124 -131 -177.5t-151 -53.5q-125 0 -208 107t-83 272q0 173 78.5 276t210.5 103q94 0 162 -57.5t124 -186.5z\" />\r\n    <glyph glyph-name=\"plusminus\" unicode=\"&#xb1;\" \r\nd=\"M88 170h1057v-170h-1057v170zM700 1171v-329h445v-170h-445v-332h-168v332h-444v170h444v329h168z\" />\r\n    <glyph glyph-name=\"lessequal\" unicode=\"&#x2264;\" \r\nd=\"M1143 438v-182l-1057 332v168l1057 331v-184l-801 -231zM1143 170v-170h-1057v170h1057z\" />\r\n    <glyph glyph-name=\"greaterequal\" unicode=\"&#x2265;\" \r\nd=\"M88 438l803 234l-803 231v184l1057 -331v-168l-1057 -332v182zM88 170h1057v-170h-1057v170z\" />\r\n    <glyph glyph-name=\"yen\" unicode=\"&#xa5;\" \r\nd=\"M37 1493h215l364 -659l363 659h217l-330 -561h252v-111h-315l-86 -151v-35h401v-111h-401v-524h-203v524h-399v111h399v35l-90 151h-309v111h243z\" />\r\n    <glyph glyph-name=\"mu\" unicode=\"&#xb5;\" \r\nd=\"M195 -428v1548h184v-696q0 -142 60 -217.5t171 -75.5q127 0 191.5 85t64.5 251v653h185v-864q0 -59 16 -87t49 -28q9 0 23.5 5t42.5 18v-148q-38 -23 -72 -34t-68 -11q-62 0 -99.5 39.5t-49.5 118.5q-46 -80 -112.5 -119t-155.5 -39q-92 0 -155.5 38t-107.5 118v-555\r\nh-167z\" />\r\n    <glyph glyph-name=\"partialdiff\" unicode=\"&#x2202;\" \r\nd=\"M842 621q15 87 22.5 168.5t7.5 156.5q0 129 -36.5 194.5t-108.5 65.5q-55 0 -127 -58t-108 -58t-60 22t-24 53q0 62 72 111t173 49q180 0 287 -155t107 -418q0 -335 -144.5 -556t-357.5 -221q-152 0 -253.5 108t-101.5 271q0 198 110.5 326.5t272.5 128.5\r\nq101 0 166 -45.5t103 -142.5zM793 494q0 116 -43.5 178.5t-122.5 62.5q-109 0 -179.5 -129t-70.5 -336q0 -116 43 -177.5t123 -61.5q109 0 179.5 130t70.5 333z\" />\r\n    <glyph glyph-name=\"summation\" unicode=\"&#x2211;\" \r\nd=\"M332 -299h753v-137h-942v92l549 919l-549 848v95h922v-140h-733l522 -803z\" />\r\n    <glyph glyph-name=\"product\" unicode=\"&#x220f;\" \r\nd=\"M152 -436v1954h929v-1954h-155v1822h-619v-1822h-155z\" />\r\n    <glyph glyph-name=\"pi\" unicode=\"&#x3c0;\" \r\nd=\"M80 1100h1073v-148h-141v-704q0 -72 24.5 -103.5t79.5 -31.5q15 0 37 2.5t29 3.5v-133q-35 -13 -72 -19t-74 -6q-113 0 -159 65.5t-46 241.5v684h-428v-952h-180v952h-143v148z\" />\r\n    <glyph glyph-name=\"integral\" unicode=\"&#x222b;\" \r\nd=\"M393 -262q106 0 120 688l3 165q12 515 107 737t297 222q80 0 130 -42t50 -107q0 -54 -32 -85.5t-87 -31.5q-42 0 -70.5 19t-39.5 54q-6 15 -10.5 51t-20.5 36q-107 0 -121 -619q-4 -193 -8 -303q-17 -479 -111.5 -686t-290.5 -207q-79 0 -129.5 41.5t-50.5 106.5\r\nq0 56 34 87.5t95 31.5q33 0 57 -14.5t39 -42.5q9 -17 14.5 -59t24.5 -42z\" />\r\n    <glyph glyph-name=\"ordfeminine\" unicode=\"&#xaa;\" \r\nd=\"M684 1104q-144 0 -206 -34t-62 -112q0 -64 43.5 -102.5t115.5 -38.5q105 0 170.5 71.5t65.5 186.5v29h-127zM954 1165v-448h-143v112q-49 -65 -114.5 -98t-147.5 -33q-129 0 -204 68t-75 184q0 129 93.5 199t265.5 70h182v4q0 90 -59.5 136t-176.5 46q-51 0 -113.5 -17.5\r\nt-129.5 -50.5v127q68 28 135.5 42t130.5 14q184 0 270 -86t86 -269zM293 592h668v-123h-668v123z\" />\r\n    <glyph glyph-name=\"ordmasculine\" unicode=\"&#xba;\" \r\nd=\"M616 1403q-102 0 -159.5 -78t-57.5 -217t57.5 -216t159.5 -77q101 0 159.5 78.5t58.5 214.5q0 139 -57.5 217t-160.5 78zM616 1520q170 0 271.5 -112t101.5 -300q0 -187 -101 -298.5t-272 -111.5q-170 0 -271 111.5t-101 298.5q0 188 101.5 300t270.5 112zM276 592h676\r\nv-123h-676v123z\" />\r\n    <glyph glyph-name=\"Omega\" unicode=\"&#x3a9;\" \r\nd=\"M74 0v172h245q-123 134 -178 278t-55 332q0 311 144 494.5t384 183.5q242 0 386.5 -183t144.5 -495q0 -188 -55.5 -332.5t-178.5 -277.5h248v-172h-463v172q120 76 187 240.5t67 386.5q0 230 -90 362.5t-244 132.5t-243.5 -132.5t-89.5 -362.5q0 -222 67 -386.5\r\nt187 -240.5v-172h-463z\" />\r\n    <glyph glyph-name=\"ae\" unicode=\"&#xe6;\" \r\nd=\"M543 442v72h-49q-169 0 -229 -45t-60 -158q0 -89 44.5 -137.5t127.5 -48.5q92 0 129 66.5t37 250.5zM1036 657v52q0 151 -38.5 217.5t-125.5 66.5t-125 -68t-38 -225v-43h327zM1200 514h-491q-1 -15 -1.5 -32.5t-0.5 -51.5q0 -161 50.5 -233t162.5 -72q79 0 143.5 25.5\r\nt114.5 76.5v-172q-55 -41 -121 -62.5t-137 -21.5q-110 0 -184.5 41t-106.5 119q-39 -80 -105.5 -120t-163.5 -40q-156 0 -237.5 86t-81.5 250q0 171 100 260.5t291 89.5h117v88q0 120 -49.5 184t-143.5 64q-56 0 -122 -21.5t-128 -60.5v168q77 35 143 51.5t126 16.5\r\nq91 0 153 -31.5t99 -95.5q33 61 99 94t155 33q174 0 246.5 -118.5t72.5 -424.5v-90z\" />\r\n    <glyph glyph-name=\"oslash\" unicode=\"&#xf8;\" \r\nd=\"M877 780l-463 -559q36 -47 86.5 -70.5t115.5 -23.5q141 0 213 109.5t72 322.5q0 52 -6 107.5t-18 113.5zM817 897q-35 48 -84.5 71t-116.5 23q-139 0 -213.5 -107t-74.5 -309q0 -48 7 -106t21 -129zM217 180q-39 71 -59.5 168.5t-20.5 210.5q0 286 123 437t356 151\r\nq100 0 179 -27.5t139 -83.5l147 176l93 -77l-164 -195q42 -66 64 -162.5t22 -218.5q0 -287 -123 -437.5t-357 -150.5q-103 0 -181.5 29.5t-135.5 89.5l-160 -186l-92 76z\" />\r\n    <glyph glyph-name=\"questiondown\" unicode=\"&#xbf;\" \r\nd=\"M549 1092h190v-154q0 -98 -30.5 -166.5t-114.5 -150.5l-90 -89q-62 -59 -85.5 -103t-23.5 -93q0 -89 65.5 -144t174.5 -55q78 0 167 34.5t187 102.5v-188q-94 -57 -189.5 -85t-199.5 -28q-186 0 -296.5 96t-110.5 257q0 76 33.5 141.5t127.5 157.5l88 86q67 64 86 106\r\nt19 105v5q2 23 2 42v123zM745 1239h-202v254h202v-254z\" />\r\n    <glyph glyph-name=\"exclamdown\" unicode=\"&#xa1;\" \r\nd=\"M516 1239v254h203v-254h-203zM516 0v655l21 357h161l21 -357v-655h-203z\" />\r\n    <glyph glyph-name=\"logicalnot\" unicode=\"&#xac;\" \r\nd=\"M88 862h1057v-491h-168v319h-889v172z\" />\r\n    <glyph glyph-name=\"radical\" unicode=\"&#x221a;\" \r\nd=\"M100 733l-41 125l291 98l219 -731l467 1471h148v-131h-47l-506 -1604h-127l-246 825z\" />\r\n    <glyph glyph-name=\"florin\" unicode=\"&#x192;\" \r\nd=\"M1200 1520v-164q-36 28 -77 42.5t-85 14.5q-102 0 -159.5 -62t-75.5 -192l-45 -311h303v-143h-328l-100 -635q-43 -266 -142.5 -381t-285.5 -115q-57 0 -108 10.5t-97 32.5v164q49 -31 99 -47.5t98 -16.5q96 0 156.5 75t86.5 226l117 687h-252v143h275l49 330\r\nq26 177 125 277.5t247 100.5q49 0 98.5 -9t100.5 -27z\" />\r\n    <glyph glyph-name=\"approxequal\" unicode=\"&#x2248;\" \r\nd=\"M1145 963v-175q-75 -59 -146.5 -86.5t-150.5 -27.5q-90 0 -203 51q-22 10 -33 14q-77 34 -128.5 46t-102.5 12q-79 0 -149.5 -29.5t-143.5 -93.5v174q78 60 151 87.5t156 27.5q53 0 103 -11t125 -43q10 -4 32 -15q116 -54 209 -54q70 0 138.5 30t142.5 93zM1145 596v-176\r\nq-75 -59 -146.5 -87t-150.5 -28q-90 0 -203 51l-33 15q-83 36 -132.5 47.5t-98.5 11.5q-78 0 -148.5 -30t-144.5 -95v174q79 62 152 89.5t155 27.5q96 0 226 -55h1l33 -15q116 -55 209 -55q68 0 137 30.5t144 94.5z\" />\r\n    <glyph glyph-name=\"Delta\" unicode=\"&#x2206;\" \r\nd=\"M-6 0l518 1423h209l520 -1423h-1247zM616 1219l-368 -1047h737z\" />\r\n    <glyph glyph-name=\"guillemotleft\" unicode=\"&#xab;\" \r\nd=\"M588 1059v-191l-301 -268l301 -268v-191l-469 418v82zM1042 1059v-191l-301 -268l301 -268v-191l-469 418v82z\" />\r\n    <glyph glyph-name=\"guillemotright\" unicode=\"&#xbb;\" \r\nd=\"M647 1059l469 -418v-82l-469 -418v191l301 268l-301 268v191zM193 1059l469 -418v-82l-469 -418v191l301 268l-301 268v191z\" />\r\n    <glyph glyph-name=\"ellipsis\" unicode=\"&#x2026;\" \r\nd=\"M80 305h252v-305h-252v305zM899 305h252v-305h-252v305zM489 305h252v-305h-252v305z\" />\r\n    <glyph glyph-name=\"nonbreakingspace\" unicode=\"&#xa0;\" \r\n />\r\n    <glyph glyph-name=\"Agrave\" unicode=\"&#xc0;\" \r\nd=\"M616 1315l-213 -764h426zM494 1493h245l457 -1493h-209l-110 389h-523l-108 -389h-209zM561 1899l197 -264h-154l-227 264h184z\" />\r\n    <glyph glyph-name=\"Atilde\" unicode=\"&#xc3;\" \r\nd=\"M616 1315l-213 -764h426zM494 1493h245l457 -1493h-209l-110 389h-523l-108 -389h-209zM612 1710l-57 33q-25 14 -40.5 19.5t-27.5 5.5q-35 0 -55 -25t-20 -70v-6h-125q0 101 51.5 160t136.5 59q36 0 66.5 -8t79.5 -35l57 -30q22 -13 39.5 -19t32.5 -6q31 0 51 25.5\r\nt20 66.5v6h125q-2 -100 -53 -159.5t-135 -59.5q-34 0 -64 8t-82 35z\" />\r\n    <glyph glyph-name=\"Otilde\" unicode=\"&#xd5;\" \r\nd=\"M905 745q0 329 -67.5 470t-221.5 141q-153 0 -220.5 -141t-67.5 -470q0 -328 67.5 -469t220.5 -141q154 0 221.5 140.5t67.5 469.5zM1116 745q0 -390 -123.5 -582t-376.5 -192t-376 191t-123 583q0 391 123.5 583t375.5 192q253 0 376.5 -192t123.5 -583zM612 1710\r\nl-57 33q-25 14 -40.5 19.5t-27.5 5.5q-35 0 -55 -25t-20 -70v-6h-125q0 101 51.5 160t136.5 59q36 0 66.5 -8t79.5 -35l57 -30q22 -13 39.5 -19t32.5 -6q31 0 51 25.5t20 66.5v6h125q-2 -100 -53 -159.5t-135 -59.5q-34 0 -64 8t-82 35z\" />\r\n    <glyph glyph-name=\"OE\" unicode=\"&#x152;\" \r\nd=\"M1217 170v-170h-605q-295 0 -417.5 166.5t-122.5 578.5q0 417 122 582.5t418 165.5h594v-170h-358v-442h328v-170h-328v-541h369zM590 1323q-177 0 -246.5 -115t-69.5 -463q0 -346 69.5 -460.5t246.5 -114.5h61v1153h-61z\" />\r\n    <glyph glyph-name=\"oe\" unicode=\"&#x153;\" \r\nd=\"M1047 657q0 11 1 30t1 28q0 145 -40 212.5t-126 67.5q-87 0 -125.5 -68.5t-38.5 -226.5v-43h328zM373 123q102 0 143 84t41 323q0 291 -40 378t-144 87q-103 0 -143 -83.5t-40 -352.5t40 -352.5t143 -83.5zM1210 514h-491v-84q0 -163 49.5 -235t161.5 -72\r\nq80 0 145.5 26.5t112.5 77.5v-172q-59 -43 -121.5 -63.5t-136.5 -20.5q-98 0 -171.5 33.5t-121.5 99.5q-52 -68 -116 -100.5t-148 -32.5q-189 0 -274 138t-85 450t85 450t274 138q89 0 153 -31t111 -96q37 62 102 94.5t152 32.5q175 0 247 -118.5t72 -424.5v-90z\" />\r\n    <glyph glyph-name=\"endash\" unicode=\"&#x2013;\" \r\nd=\"M0 633h1233v-141h-1233v141z\" />\r\n    <glyph glyph-name=\"emdash\" unicode=\"&#x2014;\" \r\nd=\"M0 633h1233v-141h-1233v141z\" />\r\n    <glyph glyph-name=\"quotedblleft\" unicode=\"&#x201c;\" \r\nd=\"M924 967h-252v207l196 382h154l-98 -382v-207zM465 967h-254v207l199 382h153l-98 -382v-207z\" />\r\n    <glyph glyph-name=\"quotedblright\" unicode=\"&#x201d;\" \r\nd=\"M768 1556h252v-206l-197 -383h-153l98 383v206zM309 1556h252v-206l-196 -383h-154l98 383v206z\" />\r\n    <glyph glyph-name=\"quoteleft\" unicode=\"&#x2018;\" \r\nd=\"M715 967h-252v207l196 382h154l-98 -382v-207z\" />\r\n    <glyph glyph-name=\"quoteright\" unicode=\"&#x2019;\" \r\nd=\"M561 1556h252v-206l-197 -383h-153l98 383v206z\" />\r\n    <glyph glyph-name=\"divide\" unicode=\"&#xf7;\" \r\nd=\"M494 395h245v-245h-245v245zM494 1135h245v-246h-245v246zM88 727h1057v-170h-1057v170z\" />\r\n    <glyph glyph-name=\"lozenge\" unicode=\"&#x25ca;\" \r\nd=\"M616 1409l-385 -817l385 -825l385 825zM616 1653l500 -1061l-500 -1069l-499 1069z\" />\r\n    <glyph glyph-name=\"ydieresis\" unicode=\"&#xff;\" \r\nd=\"M858 360l-117 -308q-99 -264 -133 -322q-46 -78 -115 -117t-161 -39h-148v154h109q81 0 127 47t117 243l-433 1102h195l332 -876l327 876h195zM319 1552h203v-202h-203v202zM711 1552h202v-202h-202v202z\" />\r\n    <glyph glyph-name=\"Ydieresis\" unicode=\"&#x178;\" \r\nd=\"M37 1493h215l364 -659l363 659h217l-479 -823v-670h-203v670zM319 1870h203v-203h-203v203zM711 1870h202v-203h-202v203z\" />\r\n    <glyph glyph-name=\"fraction\" unicode=\"&#x2215;\" \r\nd=\"M889 1493h190l-786 -1683h-191z\" />\r\n    <glyph glyph-name=\"currency\" unicode=\"&#xa4;\" \r\nd=\"M868 643q0 90 -62 151.5t-153 61.5q-90 0 -152.5 -61.5t-62.5 -151.5q0 -92 62.5 -154.5t152.5 -62.5q91 0 153 63t62 154zM844 924l166 166l90 -93l-166 -166q30 -49 44.5 -95.5t14.5 -94.5q0 -50 -15 -95.5t-46 -90.5l168 -166l-94 -94l-166 167q-46 -31 -91.5 -46\r\nt-95.5 -15q-48 0 -96 14.5t-96 42.5l-164 -163l-92 90l166 166q-31 51 -45.5 97.5t-14.5 92.5q0 50 15.5 96.5t46.5 91.5l-168 166l94 95l166 -168q46 31 91.5 46t96.5 15q46 0 93 -14.5t98 -44.5z\" />\r\n    <glyph glyph-name=\"guilsinglleft\" unicode=\"&#x2039;\" \r\nd=\"M815 1059v-191l-301 -268l301 -268v-191l-469 418v82z\" />\r\n    <glyph glyph-name=\"guilsinglright\" unicode=\"&#x203a;\" \r\nd=\"M420 1059l469 -418v-82l-469 -418v191l301 268l-301 268v191z\" />\r\n    <glyph glyph-name=\"fi\" unicode=\"&#xfb01;\" \r\nd=\"M895 1554h184v-233h-184v233zM776 1556v-153h-180q-99 0 -137.5 -40.5t-38.5 -143.5v-101h659v-1118h-184v977h-475v-977h-184v977h-201v143h201v78q0 184 84.5 271t263.5 87h192z\" />\r\n    <glyph glyph-name=\"fl\" unicode=\"&#xfb02;\" \r\nd=\"M1079 1556v-1556h-184v1403h-299q-99 0 -137.5 -40.5t-38.5 -143.5v-101h293v-141h-293v-977h-184v977h-201v143h201v78q0 184 84.5 271t263.5 87h495z\" />\r\n    <glyph glyph-name=\"daggerdbl\" unicode=\"&#x2021;\" \r\nd=\"M1071 223h-366v-420h-177v420h-366v154h366v543h-366v153h366v420h177v-420h366v-153h-366v-543h366v-154z\" />\r\n    <glyph glyph-name=\"periodcentered\" unicode=\"&#xb7;\" \r\nd=\"M489 864h252v-305h-252v305z\" />\r\n    <glyph glyph-name=\"quotesinglbase\" unicode=\"&#x201a;\" \r\nd=\"M502 303h252v-207l-197 -383h-154l99 383v207z\" />\r\n    <glyph glyph-name=\"quotedblbase\" unicode=\"&#x201e;\" \r\nd=\"M768 303h252v-207l-197 -383h-153l98 383v207zM309 303h252v-207l-196 -383h-154l98 383v207z\" />\r\n    <glyph glyph-name=\"perthousand\" unicode=\"&#x2030;\" \r\nd=\"M72 567l-37 97l1044 415l39 -96zM166 289q0 -71 47.5 -119.5t116.5 -48.5q68 0 117 49.5t49 118.5q0 67 -49.5 116.5t-116.5 49.5q-69 0 -116.5 -48.5t-47.5 -117.5zM45 289q0 121 82.5 203.5t202.5 82.5t203 -83t83 -203q0 -121 -83.5 -205t-202.5 -84q-121 0 -203 83\r\nt-82 206zM121 1145q0 -69 48.5 -117.5t117.5 -48.5t117.5 48.5t48.5 117.5q0 67 -49.5 116.5t-116.5 49.5q-69 0 -117.5 -48.5t-48.5 -117.5zM0 1145q0 120 83 203.5t204 83.5q120 0 203 -83.5t83 -203.5q0 -121 -83 -205t-203 -84q-121 0 -204 83.5t-83 205.5zM780 289\r\nq0 -71 48.5 -119.5t117.5 -48.5q70 0 118 48.5t48 119.5q0 67 -49 116.5t-117 49.5q-69 0 -117.5 -48.5t-48.5 -117.5zM659 289q0 120 83 203t204 83q120 0 203.5 -83t83.5 -203q0 -121 -83.5 -205t-203.5 -84q-121 0 -204 83.5t-83 205.5z\" />\r\n    <glyph glyph-name=\"Acircumflex\" unicode=\"&#xc2;\" \r\nd=\"M616 1315l-213 -764h426zM494 1493h245l457 -1493h-209l-110 389h-523l-108 -389h-209zM522 1901h189l211 -266h-140l-166 178l-165 -178h-140z\" />\r\n    <glyph glyph-name=\"Ecircumflex\" unicode=\"&#xca;\" \r\nd=\"M197 1493h886v-170h-684v-442h654v-170h-654v-541h703v-170h-905v1493zM540 1901h189l211 -266h-140l-166 178l-165 -178h-140z\" />\r\n    <glyph glyph-name=\"Aacute\" unicode=\"&#xc1;\" \r\nd=\"M616 1315l-213 -764h426zM494 1493h245l457 -1493h-209l-110 389h-523l-108 -389h-209zM672 1899h186l-229 -264h-154z\" />\r\n    <glyph glyph-name=\"Edieresis\" unicode=\"&#xcb;\" \r\nd=\"M197 1493h886v-170h-684v-442h654v-170h-654v-541h703v-170h-905v1493zM337 1870h203v-203h-203v203zM729 1870h202v-203h-202v203z\" />\r\n    <glyph glyph-name=\"Egrave\" unicode=\"&#xc8;\" \r\nd=\"M197 1493h886v-170h-684v-442h654v-170h-654v-541h703v-170h-905v1493zM579 1899l197 -264h-154l-227 264h184z\" />\r\n    <glyph glyph-name=\"Iacute\" unicode=\"&#xcd;\" \r\nd=\"M201 1493h829v-170h-313v-1153h313v-170h-829v170h313v1153h-313v170zM672 1899h186l-229 -264h-154z\" />\r\n    <glyph glyph-name=\"Icircumflex\" unicode=\"&#xce;\" \r\nd=\"M201 1493h829v-170h-313v-1153h313v-170h-829v170h313v1153h-313v170zM522 1901h189l211 -266h-140l-166 178l-165 -178h-140z\" />\r\n    <glyph glyph-name=\"Idieresis\" unicode=\"&#xcf;\" \r\nd=\"M201 1493h829v-170h-313v-1153h313v-170h-829v170h313v1153h-313v170zM319 1870h203v-203h-203v203zM711 1870h202v-203h-202v203z\" />\r\n    <glyph glyph-name=\"Igrave\" unicode=\"&#xcc;\" \r\nd=\"M201 1493h829v-170h-313v-1153h313v-170h-829v170h313v1153h-313v170zM561 1899l197 -264h-154l-227 264h184z\" />\r\n    <glyph glyph-name=\"Oacute\" unicode=\"&#xd3;\" \r\nd=\"M905 745q0 329 -67.5 470t-221.5 141q-153 0 -220.5 -141t-67.5 -470q0 -328 67.5 -469t220.5 -141q154 0 221.5 140.5t67.5 469.5zM1116 745q0 -390 -123.5 -582t-376.5 -192t-376 191t-123 583q0 391 123.5 583t375.5 192q253 0 376.5 -192t123.5 -583zM672 1899h186\r\nl-229 -264h-154z\" />\r\n    <glyph glyph-name=\"Ocircumflex\" unicode=\"&#xd4;\" \r\nd=\"M905 745q0 329 -67.5 470t-221.5 141q-153 0 -220.5 -141t-67.5 -470q0 -328 67.5 -469t220.5 -141q154 0 221.5 140.5t67.5 469.5zM1116 745q0 -390 -123.5 -582t-376.5 -192t-376 191t-123 583q0 391 123.5 583t375.5 192q253 0 376.5 -192t123.5 -583zM522 1901h189\r\nl211 -266h-140l-166 178l-165 -178h-140z\" />\r\n    <glyph glyph-name=\"Ograve\" unicode=\"&#xd2;\" \r\nd=\"M905 745q0 329 -67.5 470t-221.5 141q-153 0 -220.5 -141t-67.5 -470q0 -328 67.5 -469t220.5 -141q154 0 221.5 140.5t67.5 469.5zM1116 745q0 -390 -123.5 -582t-376.5 -192t-376 191t-123 583q0 391 123.5 583t375.5 192q253 0 376.5 -192t123.5 -583zM561 1899\r\nl197 -264h-154l-227 264h184z\" />\r\n    <glyph glyph-name=\"Uacute\" unicode=\"&#xda;\" \r\nd=\"M147 573v920h203v-1012q0 -109 6 -155.5t21 -71.5q32 -59 92.5 -89t146.5 -30q87 0 147 30t93 89q15 25 21 71t6 154v94v920h202v-920q0 -229 -28.5 -325.5t-98.5 -159.5q-66 -59 -151 -88t-191 -29q-105 0 -190 29t-152 88q-69 62 -98 160.5t-29 324.5zM672 1899h186\r\nl-229 -264h-154z\" />\r\n    <glyph glyph-name=\"Ucircumflex\" unicode=\"&#xdb;\" \r\nd=\"M147 573v920h203v-1012q0 -109 6 -155.5t21 -71.5q32 -59 92.5 -89t146.5 -30q87 0 147 30t93 89q15 25 21 71t6 154v94v920h202v-920q0 -229 -28.5 -325.5t-98.5 -159.5q-66 -59 -151 -88t-191 -29q-105 0 -190 29t-152 88q-69 62 -98 160.5t-29 324.5zM522 1901h189\r\nl211 -266h-140l-166 178l-165 -178h-140z\" />\r\n    <glyph glyph-name=\"Ugrave\" unicode=\"&#xd9;\" \r\nd=\"M147 573v920h203v-1012q0 -109 6 -155.5t21 -71.5q32 -59 92.5 -89t146.5 -30q87 0 147 30t93 89q15 25 21 71t6 154v94v920h202v-920q0 -229 -28.5 -325.5t-98.5 -159.5q-66 -59 -151 -88t-191 -29q-105 0 -190 29t-152 88q-69 62 -98 160.5t-29 324.5zM561 1899\r\nl197 -264h-154l-227 264h184z\" />\r\n    <glyph glyph-name=\"dotlessi\" unicode=\"&#x131;\" \r\nd=\"M256 1120h471v-977h365v-143h-914v143h365v834h-287v143z\" />\r\n    <glyph glyph-name=\"circumflex\" unicode=\"&#x2c6;\" \r\nd=\"M543 1638h147l246 -376h-139l-181 245l-180 -245h-139z\" />\r\n    <glyph glyph-name=\"tilde\" unicode=\"&#x2dc;\" \r\nd=\"M612 1370l-57 55q-21 20 -37.5 29.5t-30.5 9.5q-38 0 -56 -37t-20 -118h-124q1 134 52 208t142 74q39 0 71 -14t69 -47l57 -55q21 -20 37.5 -29.5t29.5 -9.5q39 0 57 37t19 118h125q-1 -134 -52 -208t-142 -74q-39 0 -71 14t-69 47z\" />\r\n    <glyph glyph-name=\"macron\" unicode=\"&#xaf;\" \r\nd=\"M317 1526h598v-148h-598v148z\" />\r\n    <glyph glyph-name=\"breve\" unicode=\"&#x2d8;\" \r\nd=\"M303 1608h119q11 -76 59 -113t135 -37q85 0 133 37t62 113h119q-11 -143 -90 -215t-224 -72q-144 0 -223 72t-90 215z\" />\r\n    <glyph glyph-name=\"dotaccent\" unicode=\"&#x2d9;\" \r\nd=\"M514 1552h205v-204h-205v204z\" />\r\n    <glyph glyph-name=\"ring\" unicode=\"&#x2da;\" \r\nd=\"M891 1524q0 -115 -79.5 -195t-195.5 -80q-115 0 -194.5 80t-79.5 195t79.5 194.5t194.5 79.5q116 0 195.5 -79.5t79.5 -194.5zM768 1524q0 63 -44 107t-108 44t-107.5 -43.5t-43.5 -107.5q0 -65 43.5 -108.5t107.5 -43.5t108 44t44 108z\" />\r\n    <glyph glyph-name=\"cedilla\" unicode=\"&#xb8;\" \r\nd=\"M700 0q56 -62 82.5 -114.5t26.5 -100.5q0 -89 -60 -134.5t-178 -45.5q-45 0 -88.5 6t-87.5 18v131q34 -17 71.5 -24.5t84.5 -7.5q58 0 88.5 24t30.5 70q0 30 -22 73.5t-66 104.5h118z\" />\r\n    <glyph glyph-name=\"hungarumlaut\" unicode=\"&#x2dd;\" \r\nd=\"M535 1638h170l-224 -376h-137zM868 1638h179l-248 -376h-135z\" />\r\n    <glyph glyph-name=\"ogonek\" unicode=\"&#x2db;\" \r\nd=\"M528 0h119q-45 -61 -66.5 -105t-21.5 -75q0 -46 27.5 -69t81.5 -23q32 0 63 7.5t62 22.5v-133q-38 -10 -72 -15t-64 -5q-122 0 -179.5 43.5t-57.5 136.5q0 48 26.5 100.5t81.5 114.5z\" />\r\n    <glyph glyph-name=\"caron\" unicode=\"&#x2c7;\" \r\nd=\"M543 1262l-246 376h139l180 -245l181 245h139l-246 -376h-147z\" />\r\n    <glyph glyph-name=\"Lslash\" unicode=\"&#x141;\" \r\nd=\"M215 1493h203v-616l315 219l78 -111l-393 -274v-541h721v-170h-924v571l-145 -106l-80 110l225 158v760z\" />\r\n    <glyph glyph-name=\"lslash\" unicode=\"&#x142;\" \r\nd=\"M639 406q0 -124 45.5 -187t134.5 -63h215v-156h-233q-165 0 -255.5 106t-90.5 300v292l-299 -209l-80 111l379 264v559h-295v144h479v-578l315 219l80 -110l-395 -275v-417z\" />\r\n    <glyph glyph-name=\"Scaron\" unicode=\"&#x160;\" \r\nd=\"M1012 1442v-205q-92 59 -184.5 89t-186.5 30q-143 0 -226 -66.5t-83 -179.5q0 -99 54.5 -151t203.5 -87l106 -24q210 -49 306 -154t96 -286q0 -213 -132 -325t-384 -112q-105 0 -211 22.5t-213 67.5v215q115 -73 217.5 -107t206.5 -34q153 0 238 68.5t85 191.5\r\nq0 112 -58.5 171t-203.5 91l-108 25q-208 47 -302 142t-94 255q0 200 134.5 320.5t357.5 120.5q86 0 181 -19.5t200 -58.5zM522 1635l-211 266h140l165 -178l166 178h140l-211 -266h-189z\" />\r\n    <glyph glyph-name=\"scaron\" unicode=\"&#x161;\" \r\nd=\"M973 1081v-180q-79 46 -159 69t-163 23q-125 0 -186.5 -40.5t-61.5 -123.5q0 -75 46 -112t229 -72l74 -14q137 -26 207.5 -104t70.5 -203q0 -166 -118 -259.5t-328 -93.5q-83 0 -174 17.5t-197 52.5v190q103 -53 197 -79.5t178 -26.5q122 0 189 49.5t67 138.5\r\nq0 128 -245 177l-8 2l-69 14q-159 31 -232 104.5t-73 200.5q0 161 109 248.5t311 87.5q90 0 173 -16.5t163 -49.5zM543 1262l-246 376h139l180 -245l181 245h139l-246 -376h-147z\" />\r\n    <glyph glyph-name=\"Zcaron\" unicode=\"&#x17d;\" \r\nd=\"M178 1493h969v-154l-780 -1169h802v-170h-1013v154l759 1169h-737v170zM522 1635l-211 266h140l165 -178l166 178h140l-211 -266h-189z\" />\r\n    <glyph glyph-name=\"zcaron\" unicode=\"&#x17e;\" \r\nd=\"M227 1122h813v-168l-643 -804h643v-150h-837v170l643 805h-619v147zM543 1262l-246 376h139l180 -245l181 245h139l-246 -376h-147z\" />\r\n    <glyph glyph-name=\"brokenbar\" unicode=\"&#xa6;\" \r\nd=\"M702 1432v-758h-172v758h172zM702 408v-758h-172v758h172z\" />\r\n    <glyph glyph-name=\"Eth\" unicode=\"&#xd0;\" \r\nd=\"M436 1493q342 0 504 -182t162 -566q0 -382 -162.5 -563.5t-503.5 -181.5h-303v709h-125v149h125v635h303zM436 166q255 0 356 125.5t101 453.5q0 331 -100.5 456.5t-356.5 125.5h-96v-469h264v-149h-264v-543h96z\" />\r\n    <glyph glyph-name=\"eth\" unicode=\"&#xf0;\" \r\nd=\"M717 1327q197 -212 288 -402t91 -390q0 -268 -125.5 -416t-354.5 -148q-228 0 -353.5 148t-125.5 416q0 265 125.5 413t349.5 148q34 0 51.5 -1t32.5 -3q-33 45 -69 89.5l-74 88.5l-279 -92l-30 98l237 80l-182 200h219l127 -145l289 94l33 -98zM793 918q-35 13 -76 19.5\r\nt-86 6.5q-146 0 -222.5 -105t-76.5 -304q0 -196 74 -302t210 -106q137 0 211 106t74 302q0 110 -29 211.5t-79 171.5z\" />\r\n    <glyph glyph-name=\"Yacute\" unicode=\"&#xdd;\" \r\nd=\"M37 1493h215l364 -659l363 659h217l-479 -823v-670h-203v670zM672 1899h186l-229 -264h-154z\" />\r\n    <glyph glyph-name=\"yacute\" unicode=\"&#xfd;\" \r\nd=\"M858 360l-117 -308q-99 -264 -133 -322q-46 -78 -115 -117t-161 -39h-148v154h109q81 0 127 47t117 243l-433 1102h195l332 -876l327 876h195zM756 1638h198l-325 -376h-154z\" />\r\n    <glyph glyph-name=\"Thorn\" unicode=\"&#xde;\" \r\nd=\"M403 1057v-525h234q158 0 236.5 66t78.5 197t-78.5 196.5t-236.5 65.5h-234zM201 1493h202v-270h254q260 0 384 -105t124 -323q0 -219 -124 -323.5t-384 -104.5h-254v-367h-202v1493z\" />\r\n    <glyph glyph-name=\"thorn\" unicode=\"&#xfe;\" \r\nd=\"M375 141v-567h-185v1993h185v-590q46 83 122.5 126.5t176.5 43.5q203 0 318.5 -157t115.5 -435q0 -273 -116 -428.5t-318 -155.5q-102 0 -178.5 43.5t-120.5 126.5zM915 559q0 214 -67.5 323t-200.5 109q-134 0 -203 -109.5t-69 -322.5q0 -212 69 -322t203 -110\r\nq133 0 200.5 109t67.5 323z\" />\r\n    <glyph glyph-name=\"minus\" unicode=\"&#x2212;\" \r\nd=\"M88 727h1057v-170h-1057v170z\" />\r\n    <glyph glyph-name=\"multiply\" unicode=\"&#xd7;\" \r\nd=\"M150 293l350 348l-350 350l116 117l350 -350l351 350l116 -117l-350 -350l348 -348l-116 -119l-349 350l-348 -350z\" />\r\n    <glyph glyph-name=\"onesuperior\" unicode=\"&#xb9;\" \r\nd=\"M362 778h205v611l-223 -41v116l229 39h138v-725h204v-110h-553v110z\" />\r\n    <glyph glyph-name=\"twosuperior\" unicode=\"&#xb2;\" \r\nd=\"M483 782h410v-114h-571v110l231 226q101 97 141.5 158t40.5 112q0 61 -50 99t-132 38q-49 0 -105.5 -18t-119.5 -54v125q65 28 125.5 42t119.5 14q142 0 228 -66.5t86 -173.5q0 -57 -37 -116.5t-156 -176.5z\" />\r\n    <glyph glyph-name=\"threesuperior\" unicode=\"&#xb3;\" \r\nd=\"M731 1120q94 -21 143.5 -75t49.5 -134q0 -124 -95 -191t-272 -67q-58 0 -115 10.5t-116 30.5v121q69 -27 125.5 -40t101.5 -13q101 0 160 40.5t59 108.5q0 74 -55 112t-164 38h-66v108h74q93 0 142 31.5t49 91.5q0 56 -48 87.5t-135 31.5q-44 0 -100 -12t-115 -35v121\r\nq69 17 127.5 26t108.5 9q145 0 229 -59t84 -158q0 -71 -44 -117.5t-128 -65.5z\" />\r\n    <glyph glyph-name=\"onequarter\" unicode=\"&#xbc;\" \r\nd=\"M104 934h205v611l-223 -41v116l229 39h138v-725h204v-110h-553v110zM51 504l-24 108l1060 262l27 -108zM815 565h162v-538h116v-111h-116v-186h-138v186h-387v121zM839 440l-274 -413h274v413z\" />\r\n    <glyph glyph-name=\"onehalf\" unicode=\"&#xbd;\" \r\nd=\"M104 934h205v611l-223 -41v116l229 39h138v-725h204v-110h-553v110zM51 504l-24 108l1060 262l27 -108zM684 -156h410v-114h-571v110l231 226q101 97 141.5 158t40.5 112q0 61 -50 99t-132 38q-49 0 -105.5 -18t-119.5 -54v125q65 28 125.5 42t119.5 14q142 0 228 -66.5\r\nt86 -173.5q0 -57 -37 -116.5t-156 -176.5z\" />\r\n    <glyph glyph-name=\"threequarters\" unicode=\"&#xbe;\" \r\nd=\"M504 1276q94 -21 143.5 -75t49.5 -134q0 -124 -95 -191t-272 -67q-58 0 -115 10.5t-116 30.5v121q69 -27 125.5 -40t101.5 -13q101 0 160 40.5t59 108.5q0 74 -55 112t-164 38h-66v108h74q93 0 142 31.5t49 91.5q0 56 -48 87.5t-135 31.5q-44 0 -100 -12t-115 -35v121\r\nq69 17 127.5 26t108.5 9q145 0 229 -59t84 -158q0 -71 -44 -117.5t-128 -65.5zM51 504l-24 108l1060 262l27 -108zM815 565h162v-538h116v-111h-116v-186h-138v186h-387v121zM839 440l-274 -413h274v413z\" />\r\n    <glyph glyph-name=\"Gbreve\" unicode=\"&#x11e;\" \r\nd=\"M1104 123q-81 -75 -182.5 -113.5t-219.5 -38.5q-284 0 -442 203.5t-158 570.5q0 366 160 570.5t445 204.5q94 0 180 -26.5t166 -80.5v-207q-81 77 -166 113.5t-180 36.5q-197 0 -295.5 -152.5t-98.5 -458.5q0 -311 95.5 -460.5t293.5 -149.5q67 0 117.5 15.5t91.5 48.5\r\nv401h-217v166h410v-643zM303 1901h119q12 -54 62 -82.5t132 -28.5q83 0 131.5 27.5t63.5 83.5h119q-11 -119 -90.5 -180.5t-223.5 -61.5t-223 61t-90 181z\" />\r\n    <glyph glyph-name=\"gbreve\" unicode=\"&#x11f;\" \r\nd=\"M858 569q0 207 -67.5 314.5t-196.5 107.5q-135 0 -206 -107.5t-71 -314.5t71.5 -315.5t207.5 -108.5q127 0 194.5 109t67.5 315zM1042 72q0 -252 -119 -382t-350 -130q-76 0 -159 14t-166 41v182q98 -46 178 -68t147 -22q149 0 217 81t68 257v8v125q-44 -94 -120 -140\r\nt-185 -46q-196 0 -313 157t-117 420q0 264 117 421t313 157q108 0 183 -43t122 -133v145h184v-1044zM303 1608h119q11 -76 59 -113t135 -37q85 0 133 37t62 113h119q-11 -143 -90 -215t-224 -72q-144 0 -223 72t-90 215z\" />\r\n    <glyph glyph-name=\"Idotaccent\" unicode=\"&#x130;\" \r\nd=\"M201 1493h829v-170h-313v-1153h313v-170h-829v170h313v1153h-313v170zM514 1872h205v-205h-205v205z\" />\r\n    <glyph glyph-name=\"Scedilla\" unicode=\"&#x15e;\" \r\nd=\"M1012 1442v-205q-92 59 -184.5 89t-186.5 30q-143 0 -226 -66.5t-83 -179.5q0 -99 54.5 -151t203.5 -87l106 -24q210 -49 306 -154t96 -286q0 -213 -132 -325t-384 -112q-105 0 -211 22.5t-213 67.5v215q115 -73 217.5 -107t206.5 -34q153 0 238 68.5t85 191.5\r\nq0 112 -58.5 171t-203.5 91l-108 25q-208 47 -302 142t-94 255q0 200 134.5 320.5t357.5 120.5q86 0 181 -19.5t200 -58.5zM700 0q56 -62 82.5 -114.5t26.5 -100.5q0 -89 -60 -134.5t-178 -45.5q-45 0 -88.5 6t-87.5 18v131q34 -17 71.5 -24.5t84.5 -7.5q58 0 88.5 24\r\nt30.5 70q0 30 -22 73.5t-66 104.5h118z\" />\r\n    <glyph glyph-name=\"scedilla\" unicode=\"&#x15f;\" \r\nd=\"M973 1081v-180q-79 46 -159 69t-163 23q-125 0 -186.5 -40.5t-61.5 -123.5q0 -75 46 -112t229 -72l74 -14q137 -26 207.5 -104t70.5 -203q0 -166 -118 -259.5t-328 -93.5q-83 0 -174 17.5t-197 52.5v190q103 -53 197 -79.5t178 -26.5q122 0 189 49.5t67 138.5\r\nq0 128 -245 177l-8 2l-69 14q-159 31 -232 104.5t-73 200.5q0 161 109 248.5t311 87.5q90 0 173 -16.5t163 -49.5zM700 0q56 -62 82.5 -114.5t26.5 -100.5q0 -89 -60 -134.5t-178 -45.5q-45 0 -88.5 6t-87.5 18v131q34 -17 71.5 -24.5t84.5 -7.5q58 0 88.5 24t30.5 70\r\nq0 30 -22 73.5t-66 104.5h118z\" />\r\n    <glyph glyph-name=\"Cacute\" unicode=\"&#x106;\" \r\nd=\"M1073 53q-77 -41 -158 -61.5t-172 -20.5q-287 0 -445.5 203t-158.5 571q0 366 159.5 570.5t444.5 204.5q91 0 172 -20.5t158 -61.5v-207q-74 61 -159 93t-171 32q-197 0 -295 -152t-98 -459q0 -306 98 -458t295 -152q88 0 172.5 32t157.5 93v-207zM762 1899h186\r\nl-229 -264h-154z\" />\r\n    <glyph glyph-name=\"cacute\" unicode=\"&#x107;\" \r\nd=\"M1061 57q-74 -43 -152.5 -64.5t-160.5 -21.5q-260 0 -406.5 156t-146.5 432t146.5 432t406.5 156q81 0 158 -21t155 -65v-193q-73 65 -146.5 94t-166.5 29q-173 0 -266 -112t-93 -320q0 -207 93.5 -319.5t265.5 -112.5q96 0 172 29.5t141 91.5v-191zM846 1638h198\r\nl-325 -376h-154z\" />\r\n    <glyph glyph-name=\"Ccaron\" unicode=\"&#x10c;\" \r\nd=\"M1073 53q-77 -41 -158 -61.5t-172 -20.5q-287 0 -445.5 203t-158.5 571q0 366 159.5 570.5t444.5 204.5q91 0 172 -20.5t158 -61.5v-207q-74 61 -159 93t-171 32q-197 0 -295 -152t-98 -459q0 -306 98 -458t295 -152q88 0 172.5 32t157.5 93v-207zM612 1635l-211 266h140\r\nl165 -178l166 178h140l-211 -266h-189z\" />\r\n    <glyph glyph-name=\"ccaron\" unicode=\"&#x10d;\" \r\nd=\"M1061 57q-74 -43 -152.5 -64.5t-160.5 -21.5q-260 0 -406.5 156t-146.5 432t146.5 432t406.5 156q81 0 158 -21t155 -65v-193q-73 65 -146.5 94t-166.5 29q-173 0 -266 -112t-93 -320q0 -207 93.5 -319.5t265.5 -112.5q96 0 172 29.5t141 91.5v-191zM633 1262l-246 376\r\nh139l180 -245l181 245h139l-246 -376h-147z\" />\r\n    <glyph glyph-name=\"dcroat\" unicode=\"&#x111;\" \r\nd=\"M858 977v309h-305v121h305v149h184v-149h191v-121h-191v-1286h-184v141q-46 -83 -122.5 -126.5t-176.5 -43.5q-203 0 -319.5 157.5t-116.5 434.5q0 273 117 428.5t319 155.5q101 0 178 -43.5t121 -126.5zM317 559q0 -214 68 -323t201 -109t202.5 110t69.5 322\r\nq0 213 -69.5 322.5t-202.5 109.5t-201 -109t-68 -323z\" />\r\n    <glyph glyph-name=\"sfthyphen\" unicode=\"&#xad;\" \r\nd=\"M356 643h521v-164h-521v164z\" />\r\n    <glyph glyph-name=\"periodcentered\" unicode=\"&#x2219;\" \r\nd=\"M489 864h252v-305h-252v305z\" />\r\n    <glyph glyph-name=\"Euro\" unicode=\"&#x20ac;\" \r\nd=\"M211 948q48 283 185 427.5t360 144.5q84 0 157.5 -20t147.5 -62v-207q-66 61 -145.5 95t-157.5 34q-146 0 -233 -104t-111 -308h481l-49 -108h-442q-2 -20 -2.5 -42.5t-0.5 -68.5q0 -15 0.5 -34t1.5 -42h361l-49 -110h-301q23 -203 110 -307.5t234 -104.5\r\nq79 0 157.5 33.5t145.5 95.5v-207q-72 -42 -146 -62t-159 -20q-226 0 -363.5 144t-181.5 428h-174l49 110h117q-1 12 -2 32q-2 45 -2 60q0 18 2 65q1 19 2 30h-166l49 108h125z\" />\r\n  </font>\r\n</defs></svg>\r\n"