-- | Drawing of covariance model (http://www.tbi.univie.ac.at/software/cmcompare/) guide trees and highlighting comparison results
-- Drawing is done with the diagrams package
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes #-}

module Bio.CMDraw
    (
     drawSingleCMComparisons,
     drawSingleCMs,
     drawCM,
     text',
     svgsize,
     diagramName,
     printCM,
     NodeIndices,
     buildRowIndexStructure,
     buildTreeIndexStructure,
     mergedSecondaryStructureVisualisation,
     perModelSecondaryStructureVisualisation
    ) where

import Diagrams.Prelude
import Bio.CMCompareResult
import Data.List
import qualified Data.Text as T
import qualified Data.Vector as V
import Bio.StockholmData
import Bio.StockholmDraw
import Diagrams.Backend.Cairo
import qualified Data.Vector.Unboxed as VU
import qualified Data.PrimitiveArray.Index.PhantomInt as PI
import Biobase.Types.Bitscore (Bitscore(..), score2Prob)
import Text.Printf
import qualified Data.Colour.SRGB.Linear as R
import Data.Maybe
import Control.Monad.State
import qualified Data.Char as C
import Graphics.SVGFonts
import Bio.StockholmFont
import qualified Biobase.SElab.CM.Types as CM
import qualified Biobase.SElab.CM.ModelStructure as CM
import Data.Either.Unwrap
import qualified Data.Map as M
import Data.Function    

-- | Draw one or more CM
drawSingleCMComparisons :: String -> Int -> Double -> String -> String -> Double -> Double -> [CM.CM] -> [Maybe StockholmAlignment] -> [CmcompareResult] -> [(QDiagram Cairo V2 Double Any,Maybe (QDiagram Cairo V2 Double Any))]
drawSingleCMComparisons modelDetail entryNumberCutoff transitionCutoff modelLayout emissiontype maxWidth scalef cms alns comparisons = diagrams
  where diagrams = map (drawCM modelDetail entryNumberCutoff transitionCutoff modelLayout emissiontype maxWidth scalef nameColorVector) zippedInput
        zippedInput = zip4 cms alns comparisonNodeLabels (V.toList colorVector)
        modelNumber = length cms
        comparisonNodeLabels = map (getComparisonNodeLabels comparisons nameColorVector) cms
        colorVector = makeColorVector modelNumber
        modelNames =  V.fromList (map (T.unpack . CM._name) cms)
        nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector

-- | Draw one or more CM
drawSingleCMs :: String -> Int -> Double -> String -> String -> Double -> Double -> [CM.CM] -> [Maybe StockholmAlignment] -> [(QDiagram Cairo V2 Double Any,Maybe (QDiagram Cairo V2 Double Any))]
drawSingleCMs modelDetail entryNumberCutoff transitionCutoff modelLayout emissiontype maxWidth scalef cms alns = map (drawCM modelDetail entryNumberCutoff transitionCutoff modelLayout emissiontype maxWidth scalef emptyColorNameVector) zippedInput
    where zippedInput = zip4 cms alns comparisonNodeLabels colorList
          comparisonNodeLabels = map getBlankComparisonNodeLabels cms
          colorList = replicate (length cms) white
          emptyColorNameVector = V.empty

-- | Draw the guide Tree of a single CM
drawCM :: String -> Int -> Double -> String -> String -> Double -> Double -> V.Vector (String,Colour Double) -> (CM.CM,Maybe StockholmAlignment,V.Vector (Int,V.Vector (Colour Double)), Colour Double) -> (QDiagram Cairo V2 Double Any,Maybe (QDiagram Cairo V2 Double Any))
drawCM modelDetail entryNumberCutoff transitionCutoff modelLayout emissiontype maxWidth scalef nameColorVector (inputCM,aln,comparisonNodeLabels,modelColor)
   | modelLayout == "tree" = ((applyAll ([bg white]) modelTreeLayout),alignmentDiagram)
   | modelLayout == "flat" = ((applyAll ([bg white]) modelFlatLayout),alignmentDiagram)
   | otherwise = ((applyAll ([bg white])modelTreeLayout),alignmentDiagram)
   where cm = fromLeft (CM._cm inputCM) -- select Flexible Model
         nodes = V.fromList (M.elems (CM._fmNodes cm))
         nodeNumber = V.length nodes
         allStates = CM._fmStates cm
         boxlength = fromIntegral (length alphabetSymbols) + 2
         alphabetSymbols = ['A','U','C','G']
         indices = V.toList (V.iterateN (nodeNumber-1) (1+) 0)
         (indexStructure,_)= runState (buildTreeIndexStructure 1 nodes indices) startState
         modelName = CM._name inputCM
         modelFlatLayout = alignTL (vcat' with {_sep=0.1} [modelHeader,nodeTransitions]) # scale scalef
         modelTreeLayout = alignTL (vcat' with {_sep=0.1} [modelHeader,nodeTreeTransitions]) #scale scalef
         nodeTreeTransitions = applyAll (arrowList ++ labelList) nodesTree
         nodeTransitions = applyAll (arrowList ++ labelList) nodesFlat
         firstInterval = fromJust (find (\(_,p,_,_,_) -> p == 0) (fst indexStructure))
         nodesTree = drawCMNodeTree modelDetail alphabetSymbols emissiontype boxlength allStates comparisonNodeLabels nodes (fst indexStructure) firstInterval
         modelHeader = makeModelHeader (T.unpack modelName) modelColor nameColorVector
         nodeIndices = V.iterateN nodeNumber (1+) 0
         nodesFlat = vcat' with {_sep=0.01} (V.toList (V.map (drawCMNode modelDetail alphabetSymbols emissiontype boxlength (0 :: Int) nodeNumber nodeNumber allStates comparisonNodeLabels nodes) nodeIndices))
         allConnectedStates = makeAllConnectedStates allStates
         highConnectedStates = V.filter (\(_,_,w) -> w >= transitionCutoff) allConnectedStates
         connectedStates = V.filter (\(stateId,targetStateIndex,_) -> stateId /= targetStateIndex) highConnectedStates
         selfConnectedStates = V.filter (\(stateId,targetStateIndex,_) -> stateId == targetStateIndex) highConnectedStates
         arrowList = case modelDetail of
                          "detailed" -> V.toList (V.map makeArrow connectedStates V.++ V.map makeSelfArrow selfConnectedStates)
                          "interval"-> map (makeArrow . indexStructureToConnections) (filter (\(acc,emit,_,_,_)-> acc /= emit)(fst indexStructure))
                          "minimal"-> V.toList (V.map makeArrow connectedStates)
                          "simple"-> V.toList (V.map makeArrow connectedStates)
                          _ -> []
         labelList = case modelDetail of
                          "detailed" -> V.toList (V.map makeLabel connectedStates V.++ V.map makeSelfLabel selfConnectedStates)
                          _ -> [] 
         alignmentDiagram = drawStockholmLinesComparisonLabel entryNumberCutoff maxWidth comparisonNodeLabels nodes aln

drawStockholmLinesComparisonLabel :: Int -> Double -> V.Vector (Int,V.Vector (Colour Double)) -> V.Vector CM.Node -> Maybe StockholmAlignment -> Maybe (QDiagram Cairo V2 Double Any)
drawStockholmLinesComparisonLabel entryNumberCutoff maxWidth comparisonNodeLabels nodes maybeAln
   | isJust maybeAln = Just alignmentVis
   | otherwise = Nothing
     where aln = fromJust maybeAln
           columnComparisonLabels = getComparisonPerColumnLabels comparisonNodeLabels nodes
           alignmentVis = drawStockholmLines entryNumberCutoff maxWidth columnComparisonLabels aln
                                      
makeAllConnectedStates :: M.Map (PI.PInt () CM.StateIndex) CM.State -> V.Vector (String,String,Double)
makeAllConnectedStates allStates = allConnectedStates
  where indexStateTuples = M.assocs allStates
        allConnectedStates = V.fromList (concatMap makeStateConnections indexStateTuples)
  
makeStateConnections :: (PI.PInt () CM.StateIndex,CM.State) -> [(String,String,Double)]
makeStateConnections (pInt,currentState) = conns
  where stateId = show (PI.getPInt pInt)
        targetBitscoreVector = VU.toList (CM._stateTransitions currentState)
        conns = map (\(target,bitscore) ->  (stateId,show(PI.getPInt target),score2Prob 1 bitscore)) targetBitscoreVector


-- | Extracts consensus secondary structure from alignment and annotates cmcompare nodes for each model-model combination seperatly
perModelSecondaryStructureVisualisation :: String -> Double -> String -> [CM.CM] -> [Maybe StockholmAlignment] -> [CmcompareResult] -> [(String,String)]
perModelSecondaryStructureVisualisation selectedTool _ structureFilePath cms alns comparisons
  | selectedTool == "forna" = fornaVis
  | selectedTool == "r2r" = r2rVis
  | selectedTool == "fornaLink" = fornaLink
  | selectedTool == "r2rfornaLink" = fornaLink ++ r2rVis
  | selectedTool == "all" = fornaLink ++ r2rVis ++ fornaVis
  | otherwise = []
  where fornaVis = concatMap (buildFornaperModelInput structureFilePath) structureComparisonInfo
        fornaLink = concatMap (buildFornaLinksInput structureFilePath) structureComparisonInfo
        r2rVis = concatMap (buildR2RperModelInput structureFilePath) structureComparisonInfo
        modelNumber = length cms
        comparisonNodeLabels = map (getComparisonPerModelNodeLabels comparisons nameColorVector) cms
        colorVector = makeColorVector modelNumber
        modelNames = V.fromList (map (T.unpack . CM._name) cms)
        nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector
        structureComparisonInfo = zip3 cms alns comparisonNodeLabels

getComparisonPerModelNodeLabels :: [CmcompareResult] -> V.Vector (String, Colour Double) -> CM.CM -> V.Vector (String,Colour Double, V.Vector (Int,V.Vector (Colour Double)))
getComparisonPerModelNodeLabels comparsionResults colorVector model = modelComparisonLabels
   where modelName = T.unpack (CM._name model)
         relevantComparisons1 = filter ((modelName==) . model1Name) comparsionResults
         modelNodeInterval1 = map (\a -> (model1Name a,model1matchednodes a))  relevantComparisons1 
         relevantComparisons2 = filter ((modelName==) . model2Name) comparsionResults
         modelNodeInterval2 = map (\a -> (model2Name a,model2matchednodes a))  relevantComparisons2
         modelNodeIntervals =  V.fromList (modelNodeInterval1 ++ modelNodeInterval2)
         nodeNumber = CM._nodesInModel model
         modelComparisonLabels = V.map (getModelComparisonLabels modelName nodeNumber colorVector) modelNodeIntervals

getModelComparisonLabels :: String -> Int -> V.Vector (String, Colour Double) -> (String,[Int])-> (String,Colour Double,V.Vector (Int,V.Vector (Colour Double)))
getModelComparisonLabels _ nodeNumber colorVector (compModel,matchedNodes) = (compModel,modelColor,comparisonNodeLabels)
  where (modelColor,modelInterval) = modelToColor colorVector (compModel,matchedNodes)
        comparisonNodeLabels = V.generate (nodeNumber +1) (makeModelComparisonNodeLabel (modelColor,modelInterval))

makeModelComparisonNodeLabel :: (Colour Double,[Int]) -> Int -> (Int,V.Vector (Colour Double))
makeModelComparisonNodeLabel (modelColor, nodeInterval) nodeNumber 
  | elem nodeNumber nodeInterval = (nodeNumber,V.singleton modelColor)
  | otherwise = (nodeNumber,V.singleton white)

getComparisonPerColumnLabels :: V.Vector (Int,V.Vector (Colour Double)) -> V.Vector CM.Node -> V.Vector (Int, V.Vector (Colour Double))
getComparisonPerColumnLabels comparisonNodeLabels nodes = columnComparisonLabels
   where unsortedColumnComparisonLabel = concatMap (nodeToColumnComparisonLabel nodes) (V.toList comparisonNodeLabels)
         columnComparisonLabels = V.fromList (sortBy (compare `on` fst) unsortedColumnComparisonLabel)

nodeToColumnComparisonLabel:: V.Vector CM.Node -> (Int, V.Vector (Colour Double)) -> [(Int,V.Vector (Colour Double))]
nodeToColumnComparisonLabel nodes (nodeIndex,colors) = colLabels
  where currentNode = (V.!) nodes nodeIndex
        colIndices = nub [CM._nodeColL currentNode,CM._nodeColR currentNode]
        colLabels = map (\a->(a,colors)) colIndices
                                   
-- -- modelNodeToColumnInterval ::
-- modelNodeToColumnInterval nodes (modelName,nodeInterval) = (modelName,columnInterval)
--   where columnInterval = concatMap (nodeToColumnInterval nodes) nodeInterval

                         
-- nodeToColumnInterval nodes nodeIndex = nub [CM._nodeColL currentNode,CM._nodeColR currentNode]
--   where currentNode = (V.!) nodes nodeIndex     


-- getModelComparisonLabels :: String -> Int -> V.Vector (String, Colour Double) -> (String,[Int])-> (String,Colour Double,V.Vector (Int,V.Vector (Colour Double)))
-- getModelComparisonLabels _ nodeNumber colorVector (compModel,matchedNodes) = (compModel,modelColor,comparisonNodeLabels)
--   where (modelColor,modelInterval) = modelToColor colorVector (compModel,matchedNodes)
--         comparisonNodeLabels = V.generate (nodeNumber +1) (makeModelComparisonNodeLabel (modelColor,modelInterval))

-- makeModelComparisonNodeLabel :: (Colour Double,[Int]) -> Int -> (Int,V.Vector (Colour Double))
-- makeModelComparisonNodeLabel (modelColor, nodeInterval) nodeNumber 
--   | elem nodeNumber nodeInterval = (nodeNumber,V.singleton modelColor)
--   | otherwise = (nodeNumber,V.singleton white)

--
buildR2RperModelInput :: String -> (CM.CM, Maybe StockholmAlignment,V.Vector (String,Colour Double,V.Vector (Int,V.Vector (Colour Double)))) -> [(String,String)]
buildR2RperModelInput structureFilePath (inputCM,maybeAln,comparisonNodeLabels)
  | isNothing maybeAln = []
  | otherwise = if V.null comparisonNodeLabels then singler2rInput else V.toList r2rComparisonInputs
  where cm = fromLeft (CM._cm inputCM) -- select Flexible Model
        modelName = T.unpack (CM._name inputCM)
        nodes = V.fromList (M.elems (CM._fmNodes cm))
        aln = fromJust maybeAln
        r2rInputPrefix = sHeader ++ sConsensusStructure ++ sConsensusSequence ++ sConsensusSequenceColor ++ sCovarianceAnnotation 
        allColumnAnnotations = columnAnnotations aln
        consensusSequenceList = map annotation (filter (\annotEntry -> tag annotEntry == T.pack "RF") allColumnAnnotations)
        consensusSequence = if null consensusSequenceList then "" else T.unpack (head consensusSequenceList)
        gapfreeConsensusSequence = map C.toUpper (filter (not . isGap) consensusSequence)
        consensusStructureList = map (convertWUSStoDotBracket . annotation) (filter (\annotEntry -> tag annotEntry == T.pack "SS_cons") allColumnAnnotations)
        consensusStructure = if null consensusStructureList then "" else extractGapfreeStructure consensusSequence (T.unpack (head consensusStructureList))
        nodeAlignmentColLIndices = V.toList $ V.map CM._nodeColL nodes
        nodeAlignmentColRIndices = V.toList $ V.map CM._nodeColR nodes
        nodeAlignmentColIndices = V.fromList $ nub (nodeAlignmentColLIndices ++ nodeAlignmentColRIndices)
        maxEntryLength = length consensusStructure
        sHeader =  "# STOCKHOLM 1.0\n"
        sConsensusStructure =     "#=GC SS_cons          " ++ consensusStructure ++ "\n"
        sConsensusSequence =      "#=GC cons             " ++ gapfreeConsensusSequence ++ "\n"
        sConsensusSequenceColor = "#=GC conss            " ++ replicate (length consensusStructure) '2' ++ "\n"
        sCovarianceAnnotation =   "#=GC cov_SS_cons      " ++ replicate (length consensusStructure) '.' ++ "\n"
        singleFilePath = structureFilePath ++ modelName ++ ".r2r"                        
        singler2rInput = [(singleFilePath,r2rInputPrefix)]
        -- for multiple comparisons we need to return different filenames and labels
        r2rComparisonInputs = V.map (buildR2RperModelComparisonInput modelName structureFilePath maxEntryLength nodeAlignmentColIndices r2rInputPrefix) comparisonNodeLabels

buildR2RperModelComparisonInput :: String -> String -> Int -> V.Vector Int -> String -> (String,Colour Double,V.Vector (Int,V.Vector (Colour Double))) -> (String,String)
buildR2RperModelComparisonInput modelName structureFilePath maxEntryLength nodeAlignmentColIndices r2rInputPrefix (compModelName,modelColor,comparisonNodeLabels) = (schemeFilePath,r2rInput)
  where schemeFilePath = structureFilePath ++ modelName ++ "." ++ compModelName ++ ".r2r"
        colIndicescomparisonNodeLabels = V.zipWith (\a b -> (a,b)) nodeAlignmentColIndices comparisonNodeLabels
        sparseComparisonColLabels = V.map nodeToColIndices colIndicescomparisonNodeLabels
        fullComparisonColLabels = fillComparisonColLabels maxEntryLength sparseComparisonColLabels
        r2rLabels = map comparisonColLabelsToR2RLabel (V.toList fullComparisonColLabels)
        sComparisonHighlight =    "#=GC R2R_LABEL        " ++ r2rLabels ++ "\n"
        backBoneColor = setBackboneColor modelColor
        sBackboneColorLabel =     "#=GF R2R shade_along_backbone s rgb:" ++ backBoneColor ++ "\n"
        --sBackboneColorLabel =     "#=GF R2R shade_along_backbone s rgb:200,0,0\n"
        r2rInput = r2rInputPrefix ++ sComparisonHighlight ++ sBackboneColorLabel

setBackboneColor :: Colour Double -> String
setBackboneColor modelColor = show ((R.channelRed currentColor)* 255) ++ "," ++ show ((R.channelGreen currentColor) * 255) ++ "," ++ show ((R.channelBlue currentColor) * 255)
  where currentColor = R.toRGB modelColor

buildFornaperModelInput :: String -> (CM.CM,Maybe StockholmAlignment,V.Vector (String,Colour Double,V.Vector (Int,V.Vector (Colour Double)))) -> [(String, String)]
buildFornaperModelInput structureFilePath (inputCM,maybeAln,comparisonNodeLabelsPerModels)
  | isNothing maybeAln = []
  | otherwise = fornaInput:colorSchemes
  where cm = fromLeft (CM._cm inputCM) -- select Flexible Model
        nodes = V.fromList (M.elems (CM._fmNodes cm))
        aln = fromJust maybeAln
        fornaString = ">" ++ modelName ++ "\n" ++ gapfreeConsensusSequence ++ "\n" ++ consensusStructure
        fornaFilePath = structureFilePath ++ modelName ++ ".forna"
        fornaInput = (fornaFilePath,fornaString)
        allColumnAnnotations = columnAnnotations aln
        consensusSequenceList = map annotation (filter (\annotEntry -> tag annotEntry == T.pack "RF") allColumnAnnotations)
        consensusSequence = if null consensusSequenceList then "" else T.unpack (head consensusSequenceList)
        gapfreeConsensusSequence = map C.toUpper (filter (not . isGap) consensusSequence)
        consensusStructureList = map (convertWUSStoDotBracket . annotation) (filter (\annotEntry -> tag annotEntry == T.pack "SS_cons") allColumnAnnotations)
        consensusStructure = if null consensusStructureList then "" else extractGapfreeStructure consensusSequence (T.unpack (head consensusStructureList))
        modelName = T.unpack (CM._name inputCM)
        nodeAlignmentColLIndices = V.toList $ V.map CM._nodeColL nodes
        nodeAlignmentColRIndices = V.toList $ V.map CM._nodeColR nodes
        nodeAlignmentColIndices = V.fromList $ nub (nodeAlignmentColLIndices ++ nodeAlignmentColRIndices)
        maxEntryLength = length consensusStructure
        colorSchemes = V.toList (V.map (makeColorScheme modelName structureFilePath nodeAlignmentColIndices maxEntryLength) comparisonNodeLabelsPerModels)

buildFornaLinksInput :: String -> (CM.CM,Maybe StockholmAlignment,V.Vector (String,Colour Double,V.Vector (Int,V.Vector (Colour Double)))) -> [(String, String)]
buildFornaLinksInput structureFilePath (inputCM,maybeAln,comparisonNodeLabelsPerModels)
  | isNothing maybeAln = []
  | otherwise = if V.null comparisonNodeLabelsPerModels then singleFornaLink else fornaComparisons
  where cm = fromLeft (CM._cm inputCM) -- select Flexible Model
        nodes = V.fromList (M.elems (CM._fmNodes cm))
        aln = fromJust maybeAln
        --http://nibiru.tbi.univie.ac.at/forna/forna.html?id=fasta&file=%3Eheader\nAACGUUAGUU\n(((....)))&colors=%3Eheader\n0\n0.1\n0.2\n0.3\n0.4\n0.5\n0.6\n0.7\n0.8\n0.9\n1
        fornaURLPrefix = "http://rna.tbi.univie.ac.at/forna/forna.html?id=fasta&file=%3Eheader\\n" ++ gapfreeConsensusSequence ++ "\\n" ++ consensusStructure 
        singleFornaLink = [(fornaFilePath,fornaURLPrefix)]
        fornaFilePath = structureFilePath ++ modelName ++ ".fornalink"
        allColumnAnnotations = columnAnnotations aln
        consensusSequenceList = map annotation (filter (\annotEntry -> tag annotEntry == T.pack "RF") allColumnAnnotations)
        consensusSequence = if null consensusSequenceList then "" else T.unpack (head consensusSequenceList)
        gapfreeConsensusSequence = map C.toUpper (filter (not . isGap) consensusSequence)
        consensusStructureList = map (convertWUSStoDotBracket . annotation) (filter (\annotEntry -> tag annotEntry == T.pack "SS_cons") allColumnAnnotations)
        consensusStructure = if null consensusStructureList then "" else extractGapfreeStructure consensusSequence (T.unpack (head consensusStructureList))
        modelName = T.unpack (CM._name inputCM)
        nodeAlignmentColLIndices = V.toList $ V.map CM._nodeColL nodes
        nodeAlignmentColRIndices = V.toList $ V.map CM._nodeColR nodes
        nodeAlignmentColIndices = V.fromList $ nub (nodeAlignmentColLIndices ++ nodeAlignmentColRIndices)
        maxEntryLength = length consensusStructure
        fornaComparisons = V.toList (V.map (makeFornaComparisonLink modelName structureFilePath fornaURLPrefix nodeAlignmentColIndices maxEntryLength) comparisonNodeLabelsPerModels)
        --fornaComparisonString = intercalate "\n" fornaComparisonLinks
        --fornaComparisons = [(fornaFilePath ,fornaComparisonString)]

makeFornaComparisonLink ::  String -> String -> String -> V.Vector Int -> Int -> (String,Colour Double,V.Vector (Int,V.Vector (Colour Double))) -> (String,String)
makeFornaComparisonLink modelName structureFilePath fornaURLPrefix nodeAlignmentColIndices maxEntryLength (compModelName,_,comparisonNodeLabelsPerModel) = (comparisonPath,comparisonLink)
  where comparisonPath = structureFilePath ++ modelName ++ "." ++ compModelName ++ ".fornalink"
        comparisonLink = fornaURLPrefix ++ labelPrefix ++ singleColorLabels 
        labelPrefix = "&colors=%3Eheader\\n"
        colIndicescomparisonNodeLabels = V.zipWith (\a b -> (a,b)) nodeAlignmentColIndices comparisonNodeLabelsPerModel
        sparseComparisonColLabels = V.map nodeToColIndices colIndicescomparisonNodeLabels
        fullComparisonColLabels = fillComparisonColLabels maxEntryLength sparseComparisonColLabels
        --forna only supports a single color per node, which has to be supplied as additional color scheme
        singleColorLabels = concatMap comparisonColLabelsToFornaLinkLabel (V.toList fullComparisonColLabels)
        
comparisonColLabelsToFornaLinkLabel :: (Int, V.Vector (Colour Double)) -> String
comparisonColLabelsToFornaLinkLabel (_,colorVector)
  | V.null colorVector = ""
  | V.head colorVector /= white =  "1\\n"
  | otherwise = "0\\n"
    
makeColorScheme ::  String -> String -> V.Vector Int -> Int -> (String,Colour Double,V.Vector (Int,V.Vector (Colour Double))) -> (String,String)
makeColorScheme modelName structureFilePath nodeAlignmentColIndices maxEntryLength (compModelName,_,comparisonNodeLabelsPerModel) = (schemeFilePath,singleColorLabels)
  where schemeFilePath = structureFilePath ++ modelName ++ "." ++ compModelName ++ ".fornacolor"
        colIndicescomparisonNodeLabels = V.zipWith (\a b -> (a,b)) nodeAlignmentColIndices comparisonNodeLabelsPerModel
        sparseComparisonColLabels = V.map nodeToColIndices colIndicescomparisonNodeLabels
        fullComparisonColLabels = fillComparisonColLabels maxEntryLength sparseComparisonColLabels
        --forna only supports a single color per node, which has to be supplied as additional color scheme
        singleColorLabels = concatMap comparisonColLabelsToFornaLabel (V.toList fullComparisonColLabels)
        
-- | Extracts consensus secondary structure from alignment and annotates cmcompare nodes for all comparisons in one merged output
mergedSecondaryStructureVisualisation :: String -> Double -> [CM.CM] -> [Maybe StockholmAlignment] -> [CmcompareResult] -> [(String,String)]
mergedSecondaryStructureVisualisation selectedTool _ cms alns comparisons
  | selectedTool == "forna" = fornaVis
  | selectedTool == "r2r" = r2rVis
  | otherwise = []
  where fornaVis = map buildMergedFornaInput structureComparisonInfo
        r2rVis = map buildMergedR2RInput structureComparisonInfo
        modelNumber = length cms
        comparisonNodeLabels = map (getComparisonNodeLabels comparisons nameColorVector) cms
        colorVector = makeColorVector modelNumber
        modelNames = V.fromList (map (T.unpack . CM._name) cms)
        nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector
        structureComparisonInfo = zip3 cms alns comparisonNodeLabels
        
buildMergedFornaInput :: (CM.CM,Maybe StockholmAlignment,V.Vector (Int, V.Vector (Colour Double))) -> (String, String)
buildMergedFornaInput (inputCM,maybeAln,comparisonNodeLabels)
  | isNothing maybeAln = ([],[])
  | otherwise = (fornaInput, colorScheme)
  where cm = fromLeft (CM._cm inputCM) -- select Flexible Model
        nodes = V.fromList (M.elems (CM._fmNodes cm))
        aln = fromJust maybeAln
        fornaInput = ">" ++ modelName ++ "\n" ++ gapfreeConsensusSequence ++ "\n" ++ consensusStructure
        allColumnAnnotations = columnAnnotations aln
        consensusSequenceList = map annotation (filter (\annotEntry -> tag annotEntry == T.pack "RF") allColumnAnnotations)
        consensusSequence = if null consensusSequenceList then "" else T.unpack (head consensusSequenceList)
        gapfreeConsensusSequence = map C.toUpper (filter (not . isGap) consensusSequence)
        consensusStructureList = map (convertWUSStoDotBracket . annotation) (filter (\annotEntry -> tag annotEntry == T.pack "SS_cons") allColumnAnnotations)
        consensusStructure = if null consensusStructureList then "" else extractGapfreeStructure consensusSequence (T.unpack (head consensusStructureList))
        modelName = T.unpack (CM._name inputCM)
        nodeAlignmentColIndices = V.map CM._nodeColL nodes
        maxEntryLength = length consensusStructure
        colIndicescomparisonNodeLabels = V.zipWith (\a b -> (a,b)) nodeAlignmentColIndices comparisonNodeLabels
        sparseComparisonColLabels = V.map nodeToColIndices colIndicescomparisonNodeLabels
        fullComparisonColLabels = fillComparisonColLabels maxEntryLength sparseComparisonColLabels
        --forna only supports a single color per node, which has to be supplied as additional color scheme
        singleColorLabels = concatMap comparisonColLabelsToFornaLabel (V.toList fullComparisonColLabels)
        colorScheme = singleColorLabels

comparisonColLabelsToFornaLabel :: (Int, V.Vector (Colour Double)) -> String
comparisonColLabelsToFornaLabel (nodeNr,colorVector)
  | V.null colorVector = ""
  | V.head colorVector /= white =  " " ++ show nodeNr ++ ":blue "
  | otherwise = ""
        
buildMergedR2RInput :: (CM.CM, Maybe StockholmAlignment,V.Vector (Int,V.Vector (Colour Double))) -> (String,String)
buildMergedR2RInput (inputCM,maybeAln,comparisonNodeLabels)
   | isNothing maybeAln = ([],[])
   | otherwise = (r2rInput,[])
  where cm = fromLeft (CM._cm inputCM) -- select Flexible Model
        nodes = V.fromList (M.elems (CM._fmNodes cm))
        aln = fromJust maybeAln
        r2rInput = sHeader ++ sConsensusStructure ++ sConsensusSequence ++ sConsensusSequenceColor ++ sCovarianceAnnotation ++ sComparisonHighlight ++ sBackboneColorLabel
        allColumnAnnotations = columnAnnotations aln
        consensusSequenceList = map annotation (filter (\annotEntry -> tag annotEntry == T.pack "RF") allColumnAnnotations)
        consensusSequence = if null consensusSequenceList then "" else T.unpack (head consensusSequenceList)
        gapfreeConsensusSequence = map C.toUpper (filter (not . isGap) consensusSequence)
        consensusStructureList = map (convertWUSStoDotBracket . annotation) (filter (\annotEntry -> tag annotEntry == T.pack "SS_cons") allColumnAnnotations)
        consensusStructure = if null consensusStructureList then "" else extractGapfreeStructure consensusSequence (T.unpack (head consensusStructureList))
        nodeAlignmentColIndices = V.map CM._nodeColL nodes
        maxEntryLength = length consensusStructure
        colIndicescomparisonNodeLabels = V.zipWith (\a b -> (a,b)) nodeAlignmentColIndices comparisonNodeLabels
        sparseComparisonColLabels = V.map nodeToColIndices colIndicescomparisonNodeLabels
        fullComparisonColLabels = fillComparisonColLabels maxEntryLength sparseComparisonColLabels
        r2rLabels = map comparisonColLabelsToR2RLabel (V.toList fullComparisonColLabels)
        sHeader =  "# STOCKHOLM 1.0\n"
        sConsensusStructure =     "#=GC SS_cons          " ++ consensusStructure ++ "\n"
        sConsensusSequence =      "#=GC cons             " ++ gapfreeConsensusSequence ++ "\n"
        sConsensusSequenceColor = "#=GC conss            " ++ replicate (length consensusStructure) '2' ++ "\n"
        sCovarianceAnnotation =   "#=GC cov_SS_cons      " ++ replicate (length consensusStructure) '.' ++ "\n"
        sComparisonHighlight =    "#=GC R2R_LABEL        " ++ r2rLabels ++ "\n"
        sBackboneColorLabel =     "#=GF R2R shade_along_backbone s rgb:200,0,0\n"

comparisonColLabelsToR2RLabel :: (Int, V.Vector (Colour Double)) -> Char
comparisonColLabelsToR2RLabel (_,colorVector)
  | V.null colorVector = '.'
  | V.head colorVector /= white = 's'
  | otherwise = '.'

nodeToColIndices :: (Int,(Int,V.Vector (Colour Double))) -> (Int,V.Vector (Colour Double))
nodeToColIndices (colIndex,(_,colors)) = (colIndex,colors)

fillComparisonColLabels :: Int -> V.Vector (Int, V.Vector (Colour Double)) ->  V.Vector (Int, V.Vector (Colour Double))
fillComparisonColLabels maxEntryLength sparseComparisonColLabels = fullComparisonColLabels
   where fullComparisonColLabels = V.generate maxEntryLength (makeFullComparisonColLabel sparseComparisonColLabels)

makeFullComparisonColLabel :: V.Vector (Int, V.Vector (Colour Double)) -> Int -> (Int, V.Vector (Colour Double))
makeFullComparisonColLabel sparseComparisonColLabels colIndex = fullComparisonColLabel
  where availableLabel = V.find (\(a,_)-> colIndex == a) sparseComparisonColLabels
        fullComparisonColLabel = fromMaybe (colIndex,V.singleton white) availableLabel

indexStructureToConnections :: (Int, Int, String, Int, Int) -> (String,String,Double)
indexStructureToConnections (acc,emit,_,_,_) = (show emit,show acc,1)

data NodeIndices = S [Int] | L [Int] | R [Int]
  deriving (Show, Eq, Ord)

startState :: ([(Int,Int,String,Int,Int)],Int)
startState = ([],0::Int)

buildRowIndexStructure :: Int -> V.Vector CM.Node -> [Int] -> State ([(Int,Int,String,Int,Int)],Int) ([(Int,Int,String,Int,Int)],Int)
buildRowIndexStructure _ _ [] = do
  (a,b) <- get
  return (a,b)
buildRowIndexStructure row nodes (currentIndex:xs) = do
  (currentInterval,parentId) <- get
  let currentNode = nodes V.! currentIndex
  let currentEnd = getIndexEnd nodes (currentIndex:xs)
  let ntype = CM._nodeType currentNode
  case ntype of
    CM.Root -> put ((row,parentId,"S,",currentIndex,currentEnd):currentInterval,parentId) -- ROOT start tree             
    CM.BegL -> put ((row,parentId,"L,",currentIndex,currentEnd):currentInterval,parentId) -- BEGL set current label
    CM.BegR -> put ((row,parentId,"R,",currentIndex,currentEnd):currentInterval,parentId) -- BEGR set current label
    CM.Bif -> put (currentInterval,parentId+1)
    _ -> put (currentInterval,parentId)
  buildRowIndexStructure row nodes xs

buildTreeIndexStructure :: Int -> V.Vector CM.Node -> [Int] -> State ([(Int,Int,String,Int,Int)],Int) ([(Int,Int,String,Int,Int)],Int)
buildTreeIndexStructure intervalId nodes (currentIndex:xs) = do
  (currentInterval,parentId) <- get
  let currentNode = nodes V.! currentIndex
  let currentEnd = getIndexEnd nodes (currentIndex:xs)
  let ntype = CM._nodeType currentNode
  let maxId = if null currentInterval then 0 else maximum $ map (\(iid,_,_,_,_)-> iid) currentInterval
  let newId = maxId +1
  let nextId = setNextId ntype intervalId newId
  case ntype of
    CM.Root -> put ((intervalId,parentId,"S,",currentIndex,currentEnd):currentInterval,parentId)
    CM.BegL -> put ((newId,parentId,"L,",currentIndex,currentEnd):currentInterval,parentId)
    CM.BegR -> put ((newId,parentId,"R,",currentIndex,currentEnd):currentInterval,parentId)
    CM.Bif -> put (currentInterval,intervalId)
    _ -> put (currentInterval,parentId)
  buildTreeIndexStructure nextId nodes xs
buildTreeIndexStructure _ _ [] = do
  (a,b) <- get
  return (a,b)
  
setNextId :: CM.NodeType -> Int -> Int -> Int
setNextId ntype intervalId newId
  | ntype == CM.Root = newId
  | ntype == CM.BegL = newId
  | ntype == CM.BegR = newId
  | otherwise = intervalId

getIndexEnd :: V.Vector CM.Node -> [Int] -> Int
getIndexEnd nodes indices
  | null indices = length nodes -1
  | ntype == CM.End = currentIndex
  | ntype == CM.Bif = currentIndex
  | otherwise = getIndexEnd nodes remainingindices
   where currentIndex = head indices
         remainingindices = tail indices
         currentNode = nodes V.! currentIndex
         ntype = CM._nodeType currentNode

makeModelHeader :: String -> Colour Double -> V.Vector (String,Colour Double) -> QDiagram Cairo V2 Double Any
makeModelHeader mName modelColor nameColorVector = strutX 2 ||| setModelName mName ||| strutX 1 ||| rect 12 12 # lw 0.1 # fc modelColor # translate (r2 (negate 0, 5)) ||| strutX 30 ||| modelLegend
  where modelLegend = makeModelLegend otherModelsNameColorVector
        otherModelsNameColorVector = V.filter ((/=mName) . fst) nameColorVector

makeModelLegend :: V.Vector (String,Colour Double) -> QDiagram Cairo V2 Double Any
makeModelLegend nameColorVector
  | V.null nameColorVector = mempty
  | otherwise = (legendHead === legendBody) <> rect boxX boxY # lw 0.1 # translate (r2 ((boxX/2)-1, negate (boxY/2) + 6))
  where legendHead = setLegendLabel "Legend:"
        legendBody = vcat (V.toList (V.map makeLegendEntry nameColorVector))
        nameLengths = V.map (length . fst) nameColorVector
        maxNameLength = fromIntegral $ V.maximum nameLengths
        entryNumber = fromIntegral $ V.length nameColorVector
        boxX = maxNameLength * 6
        boxY = entryNumber * 15 + 10

makeLegendEntry :: (String,Colour Double) -> QDiagram Cairo V2 Double Any
makeLegendEntry (mName,mColor) = setLegendLabel mName ||| strutX 0.5 ||| rect 4 4 # lw 0.1 # fc mColor # translate (r2 (negate 0, 2))

setLabel :: String -> QDiagram Cairo V2 Double Any
setLabel t = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False 2 2) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 0.75, negate 0.25))
setTransition :: String -> QDiagram Cairo V2 Double Any
setTransition t = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False 2 2) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 0.75, negate 0.75))   # rotate (1/4 @@ turn)
setState :: String -> Double -> Double -> QDiagram Cairo V2 Double Any
setState t x y = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False 3 3) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (x, y))
setNodeNumber :: String -> QDiagram Cairo V2 Double Any
setNodeNumber t = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False 5 5) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 2, 0))
setNodeLabel :: String -> QDiagram Cairo V2 Double Any
setNodeLabel t = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False 6 6) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 2, 0))
setLegendLabel :: String -> QDiagram Cairo V2 Double Any
setLegendLabel t = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False 10 10) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 0.75, negate 0.75))
setModelName :: String -> QDiagram Cairo V2 Double Any
setModelName t = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False 20 20) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 0.75, negate 0.75))

drawCMNodeTree :: String -> String -> String -> Int -> M.Map (PI.PInt () CM.StateIndex) CM.State -> V.Vector (Int, V.Vector (Colour Double))-> V.Vector CM.Node -> [(Int, Int, String, Int, Int)] -> (Int,Int,String,Int,Int) -> QDiagram Cairo V2 Double Any
drawCMNodeTree modelDetail alphabetSymbols emissiontype boxlength allStates comparisonNodeLabels nodes indexStructure (intervalId,parentId,intervalType,currentIndex,currentEnd) = nodeTree
  where nodeTree = currentIntervalDrawing === hcat' with {_sep = 20} (map (drawCMNodeTree modelDetail alphabetSymbols emissiontype boxlength allStates comparisonNodeLabels nodes indexStructure) nextIntervals)
        nextIntervals = filter (\(_,p,_,_,_) -> intervalId == p) indexStructure
        currentIntervalDrawing = drawCMNodeInterval modelDetail alphabetSymbols emissiontype boxlength currentIndex currentEnd currentEnd allStates comparisonNodeLabels nodes (intervalId,parentId,intervalType,currentIndex,currentEnd)  --- ||| (text' (show intervalId ++ "I" ++ show indexStructure) <> rect 100 100)

--drawCMNodeRow :: String -> String -> String -> Int -> Int -> Int -> Int -> M.Map (PI.PInt () CM.StateIndex) CM.State -> V.Vector (Int, V.Vector (Colour Double))-> V.Vector CM.Node -> [(Int, Int, String, Int, Int)] -> QDiagram Cairo V2 Double Any
--drawCMNodeRow modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes intervals = strutY 4 === hcat' with { _sep = 8 } (map (drawCMNodeInterval modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes) intervals)

drawCMNodeInterval :: String -> String -> String -> Int -> Int -> Int -> Int -> M.Map (PI.PInt () CM.StateIndex) CM.State -> V.Vector (Int, V.Vector (Colour Double))-> V.Vector CM.Node -> (Int, Int, String, Int, Int) -> QDiagram Cairo V2 Double Any
drawCMNodeInterval modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes (intervalId,_,_,currentIndex,currentEnd)
  | modelDetail == "interval" = intervalVis
  | otherwise = nodeVis
  where intervalVis = rect 20 0 # named ("a" ++ intervalIdString)  # lw 0.0 === (rect 20 40 # lw 0.1 <> text' (show currentIndex ++ "-" ++ show currentEnd)) === rect 20 0 # named ("e" ++ intervalIdString)  # lw 0.0 === strutY 5.0
        intervalIdString = show intervalId
        nodeVis = strutY 4  ===vcat' with { _sep = nodespacer } (map (drawCMNode modelDetail alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex states comparisonNodeLabels nodes) currentInterval)
        currentInterval = [currentIndex..currentEnd]
        nodespacer = if modelDetail == "detailed" then (0 :: Double) else (0.5 :: Double)

getCMNodeType :: CM.Node -> String
getCMNodeType node
  | ntype == CM.Bif = "BIF"
  | ntype == CM.MatP = "MATP"
  | ntype == CM.MatL = "MATL"
  | ntype == CM.MatR = "MATR"
  | ntype == CM.BegL = "BEGL"
  | ntype == CM.BegR = "BEGR"
  | ntype == CM.Root = "ROOT"
  | ntype == CM.End = "END"
  | otherwise = "NA"
    where ntype = CM._nodeType node

text' :: String -> QDiagram Cairo V2 Double Any
text' t = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False 3 3) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 0.75, negate 0.75))

--textWithSize' :: String -> Double -> QDiagram Cairo V2 Double Any
--textWithSize' t si = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False si si) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate siOffset, negate siOffset))
--  where siOffset = si/2

-- | Transform covariance model node labels to colors
--labelToColor :: String -> Colour Double
--labelToColor label
--   | label == "MATP" = sRGB24 211 211 211 -- P
--   | label == "MATL" = sRGB24 211 211 211 -- L
--   | label == "MATR" = sRGB24 211 211 211 -- R
--   | label == "BIF"  = sRGB24 255 069 064 -- B
--   | label == "ROOT" = sRGB24 245 245 245 -- S
--   | label == "BEGL" = sRGB24 211 211 211 -- S
--   | label == "BEGR" = sRGB24 211 211 211 -- S 
--   | label == "END"  = sRGB24 245 245 245 -- E
--labelToColor _ = sRGB24 245 245 245

drawCMNode :: String -> String -> String -> Int -> Int -> Int -> Int -> M.Map (PI.PInt () CM.StateIndex) CM.State -> V.Vector (Int, V.Vector (Colour Double)) -> V.Vector CM.Node -> Int -> QDiagram Cairo V2 Double Any
drawCMNode modelDetail alphabetSymbols emissiontype boxlength _ _ _ states comparisonNodeLabels nodes nodeIndex
  | modelDetail == "minimal" = drawCMMinimalNodeBox alphabetSymbols emissiontype boxlength states comparisonNodeLabels node nodeIndex
  | modelDetail == "simple" = drawCMSimpleNodeBox alphabetSymbols emissiontype boxlength states comparisonNodeLabels node nodeIndex
  | otherwise = detailedNodeBox
  where node = nodes V.! nodeIndex
        --idNumber = nodeIndex
        --nId = show idNumber
        detailedNodeBox = drawCMDetailedNodeBox alphabetSymbols emissiontype boxlength states comparisonNodeLabels node nodeIndex
        --nodeType = getCMNodeType node
        --nodeLabels = V.toList (snd (comparisonNodeLabels V.! idNumber))

colorBox :: Double -> Colour Double -> QDiagram Cairo V2 Double Any
colorBox singleBoxYLength colColour = rect 5 singleBoxYLength # fc colColour # lw 0.1

drawCMMinimalNodeBox :: String -> String -> Int -> M.Map (PI.PInt () CM.StateIndex) CM.State -> V.Vector (Int, V.Vector (Colour Double)) -> CM.Node -> Int -> QDiagram Cairo V2 Double Any
drawCMMinimalNodeBox alphabetSymbols emissiontype boxlength currentStates comparisonNodeLabels node nodeIndex
  | ntype == CM.Bif = minimalNode === splitStatesBox -- bifNode 
  | ntype == CM.BegL = splitStatesBox === minimalNode -- begLNode 
  | ntype == CM.BegR = splitStatesBox === minimalNode -- begRNode 
  | otherwise = minimalNode
    where ntype = CM._nodeType node
          idNumber = nodeIndex
          nId = show idNumber
          stateIndices = V.toList (CM._nodeStates node)
          minimalNode = rect 5 5 # lw 0.1 # lc black <> text' nId # fontSize 2  <> wheel 2 nodeLabels # lw 0.1 # lc black
          splitStatesBox = hcat (map (drawCMSimpleStateBox nId alphabetSymbols emissiontype boxlength currentStates) stateIndices)
          --nodeType = getCMNodeType node
          nodeLabels = V.toList (snd (comparisonNodeLabels V.! idNumber))
          --boxNumber = fromIntegral $ length nodeLabels
          --totalBoxYlength = 5 
          --singleBoxYLength = totalBoxYlength / boxNumber
          --colourBoxes = vcat (map (colorBox singleBoxYLength) nodeLabels)

drawCMSimpleNodeBox :: String -> String -> Int -> M.Map (PI.PInt () CM.StateIndex) CM.State -> V.Vector (Int, V.Vector (Colour Double)) -> CM.Node -> Int -> QDiagram Cairo V2 Double Any
drawCMSimpleNodeBox alphabetSymbols emissiontype boxlength currentStates comparisonNodeLabels node nodeIndex
  | ntype == CM.Bif = simpleNode === splitStatesBox -- bifNode 
  | ntype == CM.BegL = splitStatesBox === simpleNode -- begLNode 
  | ntype == CM.BegR = splitStatesBox === simpleNode -- begRNode 
  | otherwise = simpleNode
    where ntype = CM._nodeType node
          idNumber = nodeIndex 
          nId = show idNumber
          stateIndices = V.toList (CM._nodeStates node)
          simpleNode = rect 10 5 # lw 0.1 # lc black <>  ((text' nId # translate (r2 (negate 7.5,0)) <> colourBoxes # translate (r2 (negate 7.5, boxYoffset))) ||| text' nodeType # translate (r2 (14,0)))
          splitStatesBox = hcat (map (drawCMSimpleStateBox nId alphabetSymbols emissiontype boxlength currentStates) stateIndices)
          nodeType = getCMNodeType node
          nodeLabels = V.toList (snd (comparisonNodeLabels V.! idNumber))
          boxNumber = fromIntegral $ length nodeLabels
          totalBoxYlength = 5 
          singleBoxYLength = totalBoxYlength / boxNumber
          -- concatenated colorboxes are placed atop the simplenode box with the first colorbox
          boxYoffset = totalBoxYlength/2 - singleBoxYLength/2
          colourBoxes = vcat (map (colorBox singleBoxYLength) nodeLabels)

drawCMSimpleStateBox :: String -> String -> String -> Int -> M.Map (PI.PInt () CM.StateIndex) CM.State -> PI.PInt () CM.StateIndex -> QDiagram Cairo V2 Double Any
drawCMSimpleStateBox _ _ _ _ currentStates sIndex
  | stype == CM.S = sState 
  | stype == CM.B = bState 
  | otherwise = mempty
    where currentState = currentStates M.! sIndex
          stype = CM._stateType currentState
          stateIndx = show (PI.getPInt sIndex)
          sState = rect 1 0.001 # lw 0 # named ("a" ++ stateIndx)
          bState = rect 1 0.001 # lw 0 # named ("e" ++ stateIndx)

drawCMDetailedNodeBox :: String -> String -> Int ->  M.Map (PI.PInt () CM.StateIndex) CM.State -> V.Vector (Int, V.Vector (Colour Double)) -> CM.Node -> Int -> QDiagram Cairo V2 Double Any
drawCMDetailedNodeBox alphabetSymbols emissiontype boxlength currentStates comparisonNodeLabels node nodeIndex
  | ntype == CM.Bif = bifNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.MatP = matPNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.MatL = matLNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.MatR = matRNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.BegL = begLNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.BegR = begRNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.Root = rootNode # translate (r2 (negate 25,25)) <> nodeBox
  | ntype == CM.End = endNode # translate (r2 (negate 25,25)) <> nodeBox
  | otherwise = endNode <> nodeBox
    where ntype = CM._nodeType node
          idNumber = nodeIndex
          nId = show idNumber
          nodeLabels = V.toList (snd (comparisonNodeLabels V.! idNumber))
          stateIndices = V.toList (CM._nodeStates node)
          splitStatesBox = hcat' with { _sep = 0.01 } (map (drawCMSplitStateBox nId alphabetSymbols emissiontype boxlength currentStates) stateIndices)
          insertStatesBox = hcat (map (drawCMInsertStateBox nId alphabetSymbols emissiontype boxlength currentStates) stateIndices)
          -- bif b
          bifNode = (idBox nId "BIF" nodeLabels # rotate (1/4 @@ turn) # translate (r2 (1, negate 17)) ||| strutX 0.5 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- matP mp ml mr d il ir
          matPNode = (idBox nId "MATP" nodeLabels # rotate (1/4 @@ turn) # translate (r2 (1, negate 17)) ||| strutX 0.5||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- matL ml d il
          matLNode = (idBox nId "MATL" nodeLabels # rotate (1/4 @@ turn) # translate (r2 (1, negate 17)) ||| strutX 0.5 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- matR mr d ir
          matRNode = (idBox nId "MATR" nodeLabels # rotate (1/4 @@ turn) # translate (r2 (1, negate 17)) ||| strutX 0.5||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- begL s
          begLNode = (idBox nId "BEGL" nodeLabels # rotate (1/4 @@ turn) # translate (r2 (1, negate 17)) ||| strutX 0.5 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- begR s il
          begRNode = (idBox nId "BEGR" nodeLabels # rotate (1/4 @@ turn) # translate (r2 (1, negate 17)) ||| strutX 0.5||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- root s il ir
          rootNode = (idBox nId "ROOT" nodeLabels # rotate (1/4 @@ turn) # translate (r2 (1, negate 17)) ||| strutX 0.5||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- end e
          endNode = (idBox nId "END" nodeLabels # rotate (1/4 @@ turn) # translate (r2 (1, negate 17)) ||| strutX 0.5 ||| splitStatesBox) === strutY 5.0 === insertStatesBox

idBox :: String -> String -> [Colour Double] -> QDiagram Cairo V2 Double Any
idBox nId nType nodeLabels = (setNodeNumber nId # translate (r2 (negate ((fromIntegral (length nId))/2), 0)) <>  wheel 4 nodeLabels # lw 0.1 # translate (r2 (0, 1)) <> rect 3 3 # lw 0) ||| strutX 1.0 ||| setNodeLabel nType
nodeBox :: QDiagram Cairo V2 Double Any
nodeBox = rect 60 60 # lw 0.1

wheel :: Double -> [Colour Double] -> QDiagram Cairo V2 Double Any
wheel wsize colors = wheel' # rotate r
   where
     wheel' = mconcat $ zipWith fc colors (iterateN n (rotate a) w)
     n = length colors
     a = 1 / fromIntegral n @@ turn
     w = wedge wsize xDir a # lwG 0
     r = (1/4 @@ turn)  ^-^  (1/(2*fromIntegral n) @@ turn)

drawCMSplitStateBox :: String -> String -> String -> Int -> M.Map (PI.PInt () CM.StateIndex) CM.State -> PI.PInt () CM.StateIndex -> QDiagram Cairo V2 Double Any
drawCMSplitStateBox _ _ emissiontype _ currentStates sIndex
  | stype == CM.D = dState # translate (r2 (negate 3,negate 1)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.MP = mpState # translate (r2 (negate 7,negate 1)) <> statebox 16.0 20.0 stateIndx
  | stype == CM.ML = mlState # translate (r2 (negate 3,negate 1)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.MR = mrState # translate (r2 (negate 3,negate 1)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.S = sState # translate (r2 (negate 3,negate 1)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.E = eState # translate (r2 (negate 3,negate 1)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.B = bState # translate (r2 (negate 3,negate 1)) <> statebox 8.0 20.0 stateIndx
  | stype == CM.EL = elState # translate (r2 (negate 3,negate 1)) <> statebox 8.0 20.0 stateIndx
  | otherwise = mempty
    where currentState = currentStates M.! sIndex
          stype = CM._stateType currentState
          stateIndx = show (PI.getPInt sIndex)
          singleEmissionBitscores = CM._stateEmissions currentState
          singleEmissionEntries = setEmissions emissiontype 4 singleEmissionBitscores
          singleSymbolsAndEmissions = zip ["A","U","G","C"] (VU.toList singleEmissionEntries)
          pairEmissionBitscores = CM._stateEmissions currentState
          pairEmissionEntries = setEmissions emissiontype 16 pairEmissionBitscores
          pairSymbolsAndEmissions = zip ["AA","AU","AG","AC","UU","UA","UG","UC","GG","GA","GU","GC","CC","CA","CU","CG"] (VU.toList pairEmissionEntries)
          pairSymbolsAndEmissions1 = take 8 pairSymbolsAndEmissions
          pairSymbolsAndEmissions2 = drop 8 pairSymbolsAndEmissions
          dState = setState ("D" ++ stateIndx) (negate 0.5) (negate 1)  === strutY 1 
          mpState = setState ("MP" ++ stateIndx) (negate 0.5) (negate 1) === strutY 1 === (vcat (map (emissionEntry emissiontype) pairSymbolsAndEmissions1) ||| strutX 0.5 ||| vcat (map (emissionEntry emissiontype) pairSymbolsAndEmissions2))
          mlState = setState ("ML" ++ stateIndx) (negate 0.5) (negate 1) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions)
          mrState = setState ("MR" ++ stateIndx) (negate 0.5) (negate 1) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions)
          sState = setState ("S" ++ stateIndx) (negate 0.5) (negate 1) === strutY 1
          eState = setState ("E" ++ stateIndx) (negate 0.5) (negate 1) === strutY 1
          bState = setState ("B" ++ stateIndx) (negate 0.5) (negate 1) === strutY 1
          elState = setState ("EL" ++ stateIndx) (negate 0.5) (negate 1) === strutY 1

drawCMInsertStateBox :: String -> String -> String -> Int -> M.Map (PI.PInt () CM.StateIndex) CM.State -> PI.PInt () CM.StateIndex -> QDiagram Cairo V2 Double Any
drawCMInsertStateBox _ _ emissiontype _ currentStates sIndex
  | stype == CM.IL = ((ilState # translate (r2 (negate 3,negate 1))) <> statebox 8.0 20.0 stateIndx) ||| strutX 38
  | stype == CM.IR = (irState # translate (r2 (negate 3,negate 1))) <> inverseStatebox 8.0 20.0 stateIndx
  | otherwise = mempty
    where currentState = currentStates M.! sIndex
          stype = CM._stateType currentState
          stateIndx = show (PI.getPInt sIndex)
          singleEmissionBitscores = CM._stateEmissions currentState
          singleEmissionEntries = setEmissions emissiontype 4 singleEmissionBitscores
          singleSymbolsAndEmissions = zip ["A","U","G","C"] (VU.toList singleEmissionEntries)
          ilState = setState ("IL" ++ stateIndx) (negate 0.5) (negate 1) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions)
          irState = setState ("IR" ++ stateIndx) (negate 0.5) (negate 1) === strutY 1 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions)

setEmissions :: String -> Double -> VU.Vector Bitscore -> VU.Vector Double
setEmissions emissiontype normalizationFactor emissions
  | emissiontype == "score" = scoreentries
  | emissiontype == "probability" = propentries
  | emissiontype == "bar" = barentries
  | otherwise = barentries
    where scoreentries = VU.map bitScore2Double emissions
          propentries = VU.map ((/normalizationFactor) . score2Prob 1) emissions
          barentries = VU.map ((/normalizationFactor) . score2Prob 1) emissions

--wrap :: a -> [a]
--wrap x = [x]

emissionEntry :: String -> (String,Double) -> QDiagram Cairo V2 Double Any
emissionEntry emissiontype (symbol,emission)
  | emissiontype == "probability" = textentry
  | emissiontype == "score" = textentry
  | emissiontype == "bar" = barentry
  | otherwise = barentry
    where textentry = setLabel (symbol ++ " " ++ printf "%.3f" emission)
          barentry = setLabel symbol  ||| strutX 0.2 ||| bar emission

bar :: Double -> QDiagram Cairo V2 Double Any
bar emission = rect (4 * emission) 1 # lw 0 # fc black # translate (r2 (negate (2 - (4 * emission/2)),0)) <> rect 4 1 # lw 0.03

--makeSingleEmissionIndices index = V.fromList [PAI.Z  PAI.:. index PAI.:. A,PAI.Z  PAI.:. index PAI.:. U,PAI.Z  PAI.:. index PAI.:. G,PAI.Z  PAI.:. index PAI.:. C]

--makePairEmissionIndices cindex = V.fromList [PAI.Z  PAI.:. cindex PAI.:. A PAI.:. A,PAI.Z  PAI.:. cindex PAI.:. A PAI.:. U,PAI.Z  PAI.:. cindex PAI.:. A PAI.:. G,PAI.Z  PAI.:. cindex PAI.:. A PAI.:. C,PAI.Z  PAI.:. cindex PAI.:. U PAI.:. U,PAI.Z  PAI.:. cindex PAI.:. U PAI.:. A,PAI.Z  PAI.:. cindex PAI.:. U PAI.:. G,PAI.Z  PAI.:. cindex PAI.:. U PAI.:. C,PAI.Z  PAI.:. cindex PAI.:. G PAI.:. G,PAI.Z  PAI.:. cindex PAI.:. G PAI.:. A,PAI.Z  PAI.:. cindex PAI.:. G PAI.:. U,PAI.Z  PAI.:. cindex PAI.:. G PAI.:. C,PAI.Z  PAI.:. cindex PAI.:. C PAI.:. C,PAI.Z  PAI.:. cindex PAI.:. C PAI.:. A,PAI.Z  PAI.:. cindex PAI.:. C PAI.:. U,PAI.Z  PAI.:. cindex PAI.:. C PAI.:.G]

statebox :: Double -> Double -> String -> QDiagram Cairo V2 Double Any
statebox x y si = (rect 0.05 0.1 # lw 0 # named ("s" ++ si) ||| rect 1 0.1 # lw 0 # named ("a" ++ si) ||| rect 2 0.1 # lw 0 ||| rect 0.05 0.1 # lw 0 # named ("z" ++ si)) === rect x y  # lw 0.1 === rect 1 0 # lw 0 # named ("e" ++ si)

inverseStatebox :: Double -> Double -> String -> QDiagram Cairo V2 Double Any
inverseStatebox x y si = (rect 0.05 0.1 # lw 0 # named ("s" ++ si) ||| rect 1 0.1 # lw 0 # named ("a" ++ si) ||| rect 2 0.1 # lw 0  ||| rect 0.05 0.1 # lw 0 # named ("z" ++ si)) === rect x y  # lw 0.1 === rect 1 0 # lw 0 # named ("e" ++ si)

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
                
printCM :: FilePath -> SizeSpec V2 Double -> QDiagram Cairo V2 Double Any -> IO ()
printCM outputName = renderCairo outputName

getBlankComparisonNodeLabels :: CM.CM -> V.Vector (Int, V.Vector (Colour Double))
getBlankComparisonNodeLabels model = comparisonNodeLabels
   where comparisonNodeLabels = V.generate (nodeNumber +1 ) makeBlankComparisonNodeLabel
         nodeNumber = CM._nodesInModel model

makeBlankComparisonNodeLabel :: Int ->  (Int,V.Vector (Colour Double))
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

modelToColor :: V.Vector (String,Colour Double) ->  (String,[Int]) -> (Colour Double,[Int])
modelToColor colorVector (mName,nInterval) = nColorInterval
  where nColorInterval = (snd (fromJust entry),nInterval)
        --nColorInterval = maybe Nothing (\a -> Just (snd a,nInterval)) entry
        entry = V.find (\(a,_)-> mName == a) colorVector

makeComparisonNodeLabel :: V.Vector (Colour Double,[Int]) -> Int -> (Int,V.Vector (Colour Double))
makeComparisonNodeLabel colorNodeIntervals nodeNumber = comparisonNodeLabel
  where relevantColorNodeIntervals = V.filter (\(_,b) -> elem nodeNumber b) colorNodeIntervals
        modelColors = V.map fst relevantColorNodeIntervals
        comparisonNodeLabel = if null modelColors then (nodeNumber,V.singleton white) else (nodeNumber,modelColors)

makeColorVector :: Int -> V.Vector (Colour Double)
makeColorVector modelNumber = V.take modelNumber colorVector
   where colorVector = V.fromList [crimson, moccasin, lime, seagreen, aqua ,darkorange ,blue, blueviolet ,brown ,burlywood ,cadetblue ,chartreuse ,chocolate ,coral ,cornflowerblue ,cornsilk ,cyan ,darkblue ,darkcyan ,darkgoldenrod ,darkgray ,darkgreen ,darkgrey ,darkkhaki ,darkmagenta ,darkolivegreen ,darkorchid ,darkred ,darksalmon ,darkseagreen ,darkslateblue ,darkslategray ,darkslategrey ,darkturquoise ,darkviolet ,deeppink ,deepskyblue ,dimgray ,dimgrey ,dodgerblue ,firebrick ,forestgreen ,fuchsia ,gainsboro ,gold ,goldenrod ,gray ,grey ,green ,greenyellow ,honeydew ,hotpink ,indianred,indigo ,ivory ,khaki ,lavender ,lavenderblush ,lawngreen ,lemonchiffon ,lime ,limegreen ,linen ,magenta ,maroon ,mediumaquamarine ,mediumblue ,mediumorchid ,mediumpurple ,mediumseagreen ,mediumslateblue ,mediumspringgreen ,mediumturquoise ,mediumvioletred ,midnightblue ,mintcream ,mistyrose ,navy ,oldlace ,olive ,olivedrab ,orange ,orangered ,orchid ,papayawhip ,peachpuff ,peru ,pink ,plum ,powderblue ,purple ,red ,rosybrown ,royalblue ,saddlebrown ,salmon ,sandybrown ,seagreen]


-- makeColorVector :: Int -> V.Vector (Colour Double)
-- makeColorVector modelNumber = V.map (\(a,b,c) -> R.rgb a b c) modelRGBTupel
--    where indexVector = V.iterateN modelNumber (1+) 0
--          stepSize = (382 :: Double) / fromIntegral modelNumber
--          modelRGBTupel = V.map (makeRGBTupel stepSize) indexVector

-- makeRGBTupel :: Double -> Int -> (Double,Double,Double)
-- makeRGBTupel stepSize modelNumber = (normA,normB,normC)
--   where  totalSize = fromIntegral modelNumber * stepSize
--          a = rgbBoundries (totalSize  - 510 + 125)
--          b = rgbBoundries (totalSize - 255 + 125)
--          c = rgbBoundries totalSize + 125 
--          normA = a/255 
--          normB = b/255
--          normC = c/255 

-- rgbBoundries :: Double -> Double
-- rgbBoundries rgbValue
--   | rgbValue>240 = 240
--   | rgbValue<125 = 125
--   | otherwise = rgbValue

-- deConsSnd :: ((PAI.Z PAI.:. PInt () CM.StateIndex) PAI.:. Int) (PInt () CM.StateIndex, Bitscore) -> String
--deConstr (PAI.Z PAI.:. a PAI.:. b) = PI.getPInt a

--getPhantom (PInt i p) = p
-- fromIS PAI.IndexStream a = a

--makeTransitionIndices n = concatMap makeTransitionSubIndices indexes
--  where indexes = [0..n]

--makeTransitionSubIndices n = map  (\subI -> ((show n,show (n+subI)),PAI.Z PAI.:. (PI.PInt n) PAI.:. subI)) subIndices
--  where subIndices = [0..4]

makeArrow :: ([Char], [Char], Double) -> QDiagram Cairo V2 Double Any -> QDiagram Cairo V2 Double Any
makeArrow (lab1,lab2,weight) = connectOutside' arrowStyle1 ("e" ++ lab1) ("a" ++ lab2)
  where arrowStyle1 = with & arrowHead .~ spike & shaftStyle %~ lw (local 0.1) & headLength .~ local 0.01 & shaftStyle %~ dashingG [weight, 0.1] 0 & headStyle %~ fc black . opacity (weight * 2)

makeSelfArrow :: ([Char], [Char], Double) -> QDiagram Cairo V2 Double Any -> QDiagram Cairo V2 Double Any  
makeSelfArrow (lab1,_,weight) = connectPerim' arrowStyle ("s" ++ lab1) ("z" ++ lab1) (5/12 @@ turn) (8/12 @@ turn)
  where arrowStyle = with  & arrowHead .~ spike & arrowShaft .~ shaft' & arrowTail .~ lineTail & tailTexture .~ solid black  & shaftStyle %~ lw (local 0.1) & headLength .~ local 0.01  & tailLength .~ 0 & shaftStyle %~ dashingG [weight, 0.3] 0 & headStyle %~ fc black . opacity (weight * 2)
        shaft' = wedge 3 xDir (2/4 @@ turn)

makeLabel :: (String, String, Double) -> QDiagram Cairo V2 Double Any -> QDiagram Cairo V2 Double Any
makeLabel (n1,n2,weight) =
  withName ("a" ++ n1) $ \b1 ->
  withName ("s" ++ n2) $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v ^/ 2)
        (xOffset,yOffset) = setLabelOffset (location b1 ^. _x) (location b2 ^. _x) (location b1 ^. _y) (location b2 ^. _y)
      in
        Diagrams.Prelude.atop (position [(midpoint # translateX xOffset # translateY yOffset, setTransition ((show (roundPos 3 weight))))])
        --Diagrams.Prelude.atop (position [(midpoint # translateX xOffset # translateY yOffset, setTransition (lclass ++"," ++ (show (roundPos 3 weight))))])         
        --Diagrams.Prelude.atop (position [(midpoint # translateX xOffset # translateY yOffset, setTransition (n1 ++"," ++ n2 ++"," ++lclass ++"," ++ (show (roundPos 3 weight))))]) 

setLabelOffset :: Double -> Double -> Double -> Double -> (Double,Double)
setLabelOffset x1 x2 y1 y2
  -- 
  | ydiff < 2 = (0,0)
  | xdiff < 2 = (negate 1,negate 1)
    -- 
  | x1 > x2 && (ydiff > 30) = (negate 1,negate 10)
  -- 
  | x1 < x2 && (ydiff > 30) = (1,negate 10)             
  -- between split and insert state of same node - left upper(A)
  | x1 > x2 && (ydiff < 30) = (negate 1,negate 12)
  -- between split and insert state of same node - right upper (B)
  | x1 < x2 && (ydiff < 30) = (0,negate 12)
  -- between same split states of different nodes (C)
  | otherwise = (0,0)
    where ydiff = abs (abs y1 - abs y2)
          xdiff = abs (abs x1 - abs x2)
                
makeSelfLabel :: (String, String, Double) -> QDiagram Cairo V2 Double Any -> QDiagram Cairo V2 Double Any
makeSelfLabel (n1,_,weight)
  | weight == 0 = mempty
  | otherwise = withName ("e" ++ n1) $ \b1 ->
                  let midpoint = location b1
                  in
                    Diagrams.Prelude.atop (position [(midpoint # translateX (negate 0.25)  # translateY 22, setTransition (show (roundPos 3 weight)))])

roundPos :: (RealFrac a) => Int -> a -> a
roundPos positions number  = fromInteger (round $ number * (10^positions)) / (10.0^^positions)

bitScore2Double :: Bitscore -> Double
bitScore2Double (Bitscore x) = x
