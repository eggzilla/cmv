-- | 
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes #-}

module Bio.HMMDraw
    (
      drawHMMER3s,
      drawHMMER3,
      drawSingleHMMER3s,
      svgsize,
      diagramName,
      printHMM,
      getComparisonsHighlightParameters,
      drawHMMComparison,
      drawSingleHMMComparison
    ) where
  
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Typeable.Internal
import qualified Bio.HMMParser as HM
import Text.Printf
import Prelude
import GHC.Float
import qualified Bio.StockholmData as S
import qualified Data.Text as T    
import Data.Maybe
import qualified Data.Vector as V
import Bio.HMMCompareResult
import Bio.StockholmDraw
import qualified Data.Colour.SRGB.Linear as R
import Data.List
import Graphics.SVGFonts
import Bio.CMFont

drawHMMComparison :: String -> Int -> String -> Double -> [HM.HMMER3] -> [Maybe S.StockholmAlignment] -> [HMMCompareResult] -> QDiagram Cairo V2 Double Any
drawHMMComparison modelDetail entryNumberCutoff emissiontype maxWidth hmms alns comparisons
   | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
   | modelDetail == "simple" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
   | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
   | otherwise = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
     where zippedInput = zip4 hmms alns comparisonNodeLabels (V.toList colorVector)
           colorVector = makeColorVector modelNumber
           modelNumber = length hmms
 	   modelNames = V.fromList (map HM.name hmms)
 	   nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector
 	   comparisonNodeLabels = map (getComparisonNodeLabels comparisons nameColorVector) hmms

drawSingleHMMComparison :: String -> Int -> String -> Double -> [HM.HMMER3] -> [Maybe S.StockholmAlignment] -> [HMMCompareResult] -> [QDiagram Cairo V2 Double Any]
drawSingleHMMComparison modelDetail entryNumberCutoff emissiontype maxWidth hmms alns comparisons
   | modelDetail == "flat" = map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput
   | modelDetail == "simple" = map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput
   | modelDetail == "detailed" = map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput
   | otherwise = map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput
     where zippedInput = zip4 hmms alns comparisonNodeLabels (V.toList colorVector)
           colorVector = makeColorVector modelNumber
           modelNumber = length hmms
 	   modelNames = V.fromList (map HM.name hmms)
 	   nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector
 	   comparisonNodeLabels = map (getComparisonNodeLabels comparisons nameColorVector) hmms
                        
-- | 
drawHMMER3s :: String -> Int -> Double -> String -> [HM.HMMER3] -> [Maybe S.StockholmAlignment] -> QDiagram Cairo V2 Double Any
drawHMMER3s modelDetail entryNumberCutoff maxWidth emissiontype hmms alns 
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | otherwise = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
    where zippedInput = zip4 hmms alns blankComparisonNodeLabels colorList
          blankComparisonNodeLabels = map getBlankComparisonNodeLabels hmms
          colorList = replicate (length hmms) white

drawSingleHMMER3s :: String -> Int -> Double -> String -> [HM.HMMER3] -> [Maybe S.StockholmAlignment] -> [QDiagram Cairo V2 Double Any]
drawSingleHMMER3s modelDetail entryNumberCutoff maxWidth emissiontype hmms alns 
  | modelDetail == "flat" = map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput
  | modelDetail == "simple" = map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput
  | modelDetail == "detailed" = map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput
  | otherwise = map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput
    where zippedInput = zip4 hmms alns blankComparisonNodeLabels colorList
          blankComparisonNodeLabels = map getBlankComparisonNodeLabels hmms
          colorList = replicate (length hmms) white


-- |
drawHMMER3 :: String -> Int -> Double -> String -> (HM.HMMER3,Maybe S.StockholmAlignment, V.Vector (Int,V.Vector (Colour Double)), Colour Double) -> QDiagram Cairo V2 Double Any
drawHMMER3 modelDetail entriesNumberCutoff maxWidth emissiontype (model,aln,comparisonNodeLabels,modelColor)
   | modelDetail == "flat" = hcat $ V.toList (V.map drawHMMNodeFlat currentNodes)
   | modelDetail == "simple" = hcat $ V.toList (V.map drawHMMNodeSimple currentNodes)
   | modelDetail == "detailed" = applyAll ([bg white]) verboseNodesAlignment
   | otherwise = hcat $ V.toList (V.map drawHMMNodeSimple currentNodes)
     where nodeNumber = fromIntegral $ length currentNodes
           --nodes with begin node
           currentNodes = HM.begin model `V.cons` HM.nodes model
           nodeAlignmentColIndices =  V.map (fromJust . HM.nma) currentNodes
           alphabet = HM.alpha model
           alphabetSymbols = HM.alphabetToSymbols alphabet           
           boxlength = (fromIntegral (length alphabetSymbols)) + 1
           nodeWidth = (6.0 :: Double)
           nodeNumberPerRow = floor (maxWidth / nodeWidth - 2)
           nodesIntervals = makeNodeIntervals nodeNumberPerRow nodeNumber
           verboseNodes = vcat' with { _sep = 2 } (V.toList (V.map (drawDetailedNodeRow alphabetSymbols emissiontype boxlength nodeNumber currentNodes comparisonNodeLabels) nodesIntervals))
           verboseNodesAlignment = alignTL (vcat' with { _sep = 5 }  [modelHeader,verboseNodes,alignmentDiagram])
           modelHeader = makeModelHeader (HM.name model) modelColor
           alignmentDiagram = if isJust aln then drawStockholmLines entriesNumberCutoff maxWidth nodeAlignmentColIndices comparisonNodeLabels (fromJust aln) else mempty
           --connectedNodes = makeConnections boxlength currentNodes
           --selfconnectedNodes = makeSelfConnections boxlength currentNodes
           --arrowList = map makeArrow connectedNodes ++ map makeSelfArrow selfconnectedNodes
           --labelList = map makeLabel connectedNodes ++ map makeSelfLabel selfconnectedNodes

makeModelHeader :: String -> Colour Double -> QDiagram Cairo V2 Double Any
makeModelHeader mName modelColor = strutX 2 ||| setTitelLetter mName ||| strutX 1 ||| rect 4 4 # lw 0.1 # fc modelColor

makeNodeIntervals :: Int -> Int -> V.Vector (Int,Int)
makeNodeIntervals nodeNumberPerRow nodeNumber = rowIntervals
  where rowVector = V.iterateN rowNumber (1+) 0 
        rowNumber = ceiling $ (fromIntegral nodeNumber) / (fromIntegral nodeNumberPerRow)
	rowIntervals = V.map (setRowInterval nodeNumberPerRow nodeNumber) rowVector

setRowInterval :: Int -> Int -> Int -> (Int,Int)
setRowInterval nodeNumberPerRow nodeNumber index = (start,safeLength)
  where start = index*nodeNumberPerRow
        length = nodeNumberPerRow 
	safeLength = if start +length >= nodeNumber then (nodeNumber - start) else length

drawDetailedNodeRow :: [Char] -> String -> Double -> Int -> V.Vector HM.HMMER3Node -> V.Vector (Int,V.Vector (Colour Double)) -> (Int, Int) -> QDiagram Cairo V2 Double Any
drawDetailedNodeRow alphabetSymbols emissiontype boxLength lastIndex allNodes comparisonNodeLabels (currentIndex,nodeSliceLength) = detailedRow
  where currentNodes = V.slice currentIndex nodeSliceLength allNodes
        withLastRowNodes = V.slice (currentIndex -1) (nodeSliceLength +1) allNodes
        detailedRow = applyAll (lastRowList ++ arrowList ++ labelList) detailedNodes
	nextIndex = currentIndex + nodeSliceLength
        detailedNodes = hcat (V.toList (V.map (drawHMMNodeVerbose alphabetSymbols emissiontype boxLength currentIndex nextIndex lastIndex comparisonNodeLabels) currentNodes))
        arrowNodes = if currentIndex > 1 then currentNodes else currentNodes
        connectedNodes = makeConnections boxLength arrowNodes
        selfConnectedNodes = makeSelfConnections boxLength arrowNodes
        arrowList = V.toList (V.map makeArrow connectedNodes V.++ V.map makeSelfArrow selfConnectedNodes)
        labelList = V.toList (V.map makeLabel connectedNodes V.++ V.map makeSelfLabel selfConnectedNodes)
        --add arrows and labels for transitions from previous row
        lastNode = if currentIndex > 1 then V.slice (currentIndex -1) 1 allNodes else V.empty
        lastRowConnections = makeLastRowConnections boxLength lastNode
        lastRowList = V.toList (V.map makeArrow lastRowConnections V.++ V.map makeLabel lastRowConnections)   

setLetter :: String -> QDiagram Cairo V2 Double Any 
--setLetter echar = alignedText 0.5 0.5 [echar] # fontSize 2 <> rect 1.0 1.0 # lw 0 -- # translate (r2 (negate 0.5, 0))
setLetter t = textWithSize' t 2.0 
setLabelLetter :: String -> QDiagram Cairo V2 Double Any
--setLabelLetter echar = alignedText 0.5 0.5 [echar] # fontSize 0.75 <> rect 0.4 0.5 # lw 0
setLabelLetter t = textWithSize' t 0.75
setTitelLetter :: String -> QDiagram Cairo V2 Double Any
--setTitelLetter echar = alignedText 0.5 0.5 [echar] # fontSize 4.0 <> rect 4.0 4.0 # lw 0
setTitelLetter t = textWithSize' t 4.0

makeLastRowConnections :: Double -> V.Vector HM.HMMER3Node -> V.Vector ([Char], [Char], Double, (Double, Double))
makeLastRowConnections boxlength currentnodes =  mm1A V.++ md1A V.++ im1A V.++ dm1A V.++ dd1A
  where mm1A = V.map makemm1A currentnodes 
        md1A = V.map (makemd1A boxlength) currentnodes
        im1A = V.map makeim1A currentnodes
        dm1A = V.map (makedm1A boxlength) currentnodes
        dd1A = V.map makedd1A currentnodes

makeConnections :: Double -> V.Vector HM.HMMER3Node -> V.Vector ([Char], [Char], Double, (Double, Double))
makeConnections boxlength currentnodes =  mm1A V.++ miA V.++ md1A V.++ im1A V.++ dm1A V.++ dd1A
  where mm1A = V.map makemm1A currentnodes 
        miA = V.map (makemiA boxlength) currentnodes
        md1A = V.map (makemd1A boxlength) currentnodes
        im1A = V.map makeim1A currentnodes
        dm1A = V.map (makedm1A boxlength) currentnodes
        dd1A = V.map makedd1A currentnodes

makeSelfConnections :: Double -> V.Vector HM.HMMER3Node -> V.Vector ([Char], [Char], Double, (Double, Double))
makeSelfConnections boxlength currentnodes = V.map (makeiiA boxlength) currentnodes

makemm1A :: HM.HMMER3Node -> (String, String, Double, (Double, Double))
makemm1A currentNode = (show ((HM.nodeId) currentNode) ++ "m", show ((HM.nodeId currentNode) + 1) ++ "m", maybe 0 ((roundPos 2) . exp . negate) (HM.m2m currentNode),(0,0.5))
makemiA :: Double -> HM.HMMER3Node -> (String, String, Double, (Double, Double))
makemiA boxlength currentNode = (show ((HM.nodeId) currentNode) ++ "m", show ((HM.nodeId currentNode)) ++ "i",  maybe 0 ((roundPos 2) . exp . negate) (HM.m2i currentNode),(0,setiayOffset boxlength))
makemd1A :: Double -> HM.HMMER3Node -> (String, String, Double, (Double, Double))
makemd1A boxlength currentNode = (show ((HM.nodeId) currentNode) ++ "m", show ((HM.nodeId currentNode) + 1) ++ "d", maybe 0 ((roundPos 2) . exp . negate) (HM.m2d currentNode),(1.5,2.0))
makeim1A :: HM.HMMER3Node -> (String, String, Double, (Double, Double))
makeim1A currentNode = (show ((HM.nodeId) currentNode) ++ "i", show ((HM.nodeId currentNode) + 1) ++ "m", maybe 0 ((roundPos 2) . exp . negate) (HM.i2m currentNode),(0,negate 0.5))
makeiiA :: Double -> HM.HMMER3Node -> (String, String, Double, (Double, Double))
makeiiA boxlength currentNode = (show ((HM.nodeId) currentNode) ++ "i", show ((HM.nodeId currentNode)) ++ "i", maybe 0 ((roundPos 2) . exp . negate) (HM.i2i currentNode),(0,3.0))
makedm1A :: Double -> HM.HMMER3Node -> (String, String, Double, (Double, Double))
makedm1A boxlength currentNode = (show ((HM.nodeId) currentNode) ++ "d", show ((HM.nodeId currentNode) + 1) ++ "m", maybe 0 ((roundPos 2) . exp . negate) (HM.d2m currentNode),(negate 1.5,2.0))
makedd1A :: HM.HMMER3Node -> (String, String, Double, (Double, Double))
makedd1A currentNode = (show ((HM.nodeId) currentNode) ++ "d", show ((HM.nodeId currentNode) + 1) ++ "d", maybe 0 ((roundPos 2) . exp . negate) (HM.d2d currentNode),(negate 1,1))

setiayOffset :: Double -> Double
setiayOffset boxlength
  | boxlength <= 10 = negate 0.3
  | otherwise = 3.5

makeArrow :: (String,String,Double,(Double,Double)) -> QDiagram Cairo V2 Double Any -> QDiagram Cairo V2 Double Any
makeArrow (lab1,lab2,weight,_) = connectOutside' arrowStyle1 (lab1) (lab2) 
  where arrowStyle1 = with & arrowHead .~ spike & shaftStyle %~ lw (local (0.1)) & headLength .~ local 0.001 & shaftStyle %~ dashingG [weight, 0.1] 0 & headStyle %~ fc black . opacity (weight * 2)
makeSelfArrow (lab1,lab2,weight,_) = connectPerim' arrowStyle lab1 lab1 (4/12 @@ turn) (2/12 @@ turn)
  where arrowStyle = with  & arrowHead .~ spike & arrowShaft .~ shaft' & arrowTail .~ lineTail & tailTexture .~ solid black  & shaftStyle %~ lw (local (0.1)) & headLength .~ local 0.001  & tailLength .~ 0.9 & shaftStyle %~ dashingG [weight, 0.1] 0 & headStyle %~ fc black . opacity (weight * 2)
        shaft' = arc xDir (-2.7/5 @@ turn) 

-- %~ lw (local (0.1 * weight))

makeLabel (n1,n2,weight,(xOffset,yOffset))=
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v ^/ 2)
    in
      atop (position [((midpoint # translateX (negate 0.25 + xOffset) # translateY (0 + yOffset)), setLabelLetter (show weight)) ])

makeSelfLabel (n1,n2,weight,(xOffset,yOffset))=
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v ^/ 2)
    in
      atop (position [(midpoint # translateX (negate 0.25 + xOffset) # translateY (0 + yOffset), setLabelLetter (show weight))])

-- | 
--drawHMMNodeFlat :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeFlat node = rect 2 2 # lw 0.1  

-- | 
--drawHMMNodeSimple :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeSimple node = rect 2 2 # lw 0.1

-- | 
--drawHMMNodeVerbose :: String -> String -> Double -> Int -> Int -> Int -> HM.HMMER3Node -> QDiagram b V2 n Any
drawHMMNodeVerbose alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex comparisonNodeLabels node 
  | idNumber == 0 = beginBox
  | idNumber == (lastIndex - 1) = nodeBox ||| endBox
  | idNumber == rowStart = rowStartBox idNumber boxlength ||| nodeBox
  | idNumber == rowEnd - 1 = nodeBox ||| rowEndBox idNumber boxlength                    
  | otherwise = nodeBox 
  where idNumber = HM.nodeId node
        nodeLabels = V.toList (snd (comparisonNodeLabels V.! idNumber))
        nid  = show idNumber
        beginBox = strutX 7 ||| (idBox nid nodeLabels === strutY 0.5 === emptyDeletions === strutY 1.5 === insertions nid === strutY 1.5 === beginState boxlength nid) ||| (strutX  4)
	nodeBox = idBox nid nodeLabels === strutY 0.5 === deletions nid === strutY 1.5 === insertions nid  === strutY 1.5 === matches alphabetSymbols emissiontype boxlength node ||| (strutX 4)
	endBox = emptyIdBox === strutY 0.5 === emptyDeletions === strutY 1.5 === emptyInsertions  === strutY 1.5 === endState boxlength idNumber ||| strutX 4

-- | idBox associates the node with its index and a tupel of  a list of modelidentifiers and the total model number
--idBox nid nodeLabels = alignedText 0 0 nid # fontSize 2  # translate (r2 ((negate ((fromIntegral ((length nid) * 2))/2)), negate 1.25)) <>  wheel nodeLabels <> rect 1.5 3 # lw 0
idBox nid nodeLabels = text' nid # translate (r2 ((negate ((fromIntegral ((length nid) * 2))/2)), negate 1.25)) <>  wheel nodeLabels <> rect 1.5 3 # lw 0   
emptyIdBox = rect 1.5 1.5 # lw 0
rowStartBox idNumber boxlength = rect 0 1.5 #lw 0.0 === rect 0 6 # lw 0.1 # named (nid ++ "d") === rect 0 6 # lw 0.1 #named (nid ++ "i") === rect 0 (boxlength + 2) # lw 0.1 # named (nid ++ "m") ||| strutX 2.5
  where nid = show (idNumber - 1)
rowEndBox idNumber boxlength = rect 0.1 1.5 #lw 0.0 === rect 0.1 6 # lw 0.1 # named (nid ++ "d") === rect 0.1 6 # lw 0.1 #named (nid ++ "i") === rect 0.1 (boxlength + 2) #lw 0.1 #named (nid ++ "m")
  where nid = show (idNumber + 1)
deletions nid =  alignedText 0 0 "D" # translate (r2 (negate 0.25,0.25)) <> circle 3 # lw 0.1 # fc white # named (nid ++ "d")
emptyDeletions = circle 3 # lw 0.0 # fc white 
insertions nid = alignedText 0 0 "I" # translate (r2 (0,0.25)) <> rect 4.2426 4.2426 # lw 0.1 # rotateBy (1/8) # fc white # named (nid ++ "i")
emptyInsertions = rect 4.2426 4.2426 # lw 0 # rotateBy (1/8) # fc white
--matches :: String -> String -> Double -> HM.HMMER3Node -> QDiagram b0 V2 Double Any
matches alphabetSymbols emissiontype boxlength node = entries # translate (r2 (negate 2.5,boxlength/2 -1)) <> outerbox # named (nid ++ "m")
  where outerbox = rect 6 boxlength # lw 0.1 # fc white
        entries = vcat (map (emissionEntry emissiontype) symbolsAndEmissions)
        symbolsAndEmissions = zip (map wrap alphabetSymbols) (V.toList emissionEntries)
        emissionEntries = setEmissions emissiontype (HM.matchEmissions node)
        nid = show $ HM.nodeId node

wheel colors = wheel' # rotate r
   where
     wheel' = mconcat $ zipWith fc colors (iterateN n (rotate a) w)
     n = length colors
     a = 1 / (fromIntegral n) @@ turn
     w = wedge 3 xDir a # lwG 0
     r = (1/4 @@ turn)  ^-^  (1/(2*(fromIntegral n)) @@ turn)

-- B → M 1 , B → I 0 , B → D 1 ; I 0 → M 1 , I 0 → I 0
beginState boxlength nid = alignedText 0.5 0.5 "BEGIN" <> outerbox # named (nid ++ "m") <> rect 6 boxlength # named (nid ++ "d")  # lw 0.1
  where outerbox = rect 6 boxlength # lw 0.1 # fc white

endState boxlength idNumber = alignedText 0.5 0.5 "END" <> outerbox # named (nid ++ "m") <> rect 6 boxlength # named (nid ++ "d")  # lw 0.1 <> rect 6 boxlength # named (nid ++ "i")  # lw 0.1
  where outerbox = rect 6 boxlength # lw 0.1 # fc white
        nid = show (idNumber + 1)

transitions boxlength node = rect 6 height # lw 0 # translate (r2(0,negate (height/2)))
  where height = (boxlength + 1 + 1 + 6 + 6)

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
    where textentry = alignedText 0 0.1 (symbol ++ " " ++ printf "%.3f" emission) # translate (r2 (negate 0.5,0)) <> (rect 2 1 # lw 0 ) 
          --barentry =  stroke (textSVG symbol 2) ||| bar emission
          barentry = (alignedText 0 0.01 symbol  # translate (r2 (negate 0.25,negate 0.3)) <> (rect 2 1 # lw 0 )) ||| bar emission

--bar :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => Double -> QDiagram b V2 n Any
bar emission = (rect (4 * emission) 1 # lw 0 # fc black # translate (r2 (negate (2 - (4 * emission/2)),0)) <> rect 4 1 # lw 0.03 )

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

printHMM :: String -> SizeSpec V2 Double -> QDiagram Cairo V2 Double Any -> IO ()
printHMM outputName = renderCairo outputName

roundPos :: (RealFrac a) => Int -> a -> a
roundPos positions number  = (fromInteger $ round $ number * (10^positions)) / (10.0^^positions)
    	 
getComparisonsHighlightParameters :: [HM.HMMER3] -> [HMMCompareResult] -> [(Int,Int,Int,Int,Int,Int,Int,Int)]
getComparisonsHighlightParameters sortedmodels comp = map (getComparisonHighlightParameters sortedmodels) comp

getComparisonHighlightParameters :: [HM.HMMER3] -> HMMCompareResult -> (Int,Int,Int,Int,Int,Int,Int,Int)
getComparisonHighlightParameters sortedmodels comp = (a,b,c,d,a,f,c,e)
  where a = 1
        b = head (model1matchednodes comp)
        c = 2
        d = head (model2matchednodes comp)
        e = last (model2matchednodes comp)
        f = last (model1matchednodes comp)


getComparisonNodeLabels :: [HMMCompareResult] -> V.Vector (String, Colour Double) -> HM.HMMER3 -> V.Vector (Int, V.Vector (Colour Double))
getComparisonNodeLabels comparsionResults colorVector model = comparisonNodeLabels
   where modelName = HM.name model 
         relevantComparisons1 = filter ((modelName==) . model1Name) comparsionResults
	 modelNodeInterval1 = map (\a -> (model2Name a,model2matchednodes a))  relevantComparisons1 
	 relevantComparisons2 = filter ((modelName==) . model2Name) comparsionResults
         modelNodeInterval2 = map (\a -> (model1Name a,model1matchednodes a))  relevantComparisons2
         modelNodeIntervals =  V.fromList (modelNodeInterval1 ++ modelNodeInterval2)
         colorNodeIntervals = V.map (modelToColor colorVector) modelNodeIntervals
         nodeNumber = length (HM.nodes model)
         comparisonNodeLabels = V.generate (nodeNumber +1) (makeComparisonNodeLabel colorNodeIntervals)
         --nodeColorLabels = map model colorNodeIntervals

getBlankComparisonNodeLabels :: HM.HMMER3 -> V.Vector (Int, V.Vector (Colour Double))
getBlankComparisonNodeLabels model = comparisonNodeLabels
   where comparisonNodeLabels = V.generate (nodeNumber +1 )  makeBlankComparisonNodeLabel
         nodeNumber = length (HM.nodes model) 

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

makeBlankComparisonNodeLabel :: Int -> (Int,(V.Vector (Colour Double)))
makeBlankComparisonNodeLabel nodeNumber = (nodeNumber,V.singleton white)

makeColorVector :: Int -> V.Vector (Colour Double)
makeColorVector modelNumber = V.map (\(a,b,c) -> R.rgb a b c) modelRGBTupel
   where indexVector = V.iterateN (modelNumber) (1+) 0
         stepSize = (765 :: Double) / (fromIntegral modelNumber)
	 modelRGBTupel = V.map (makeRGBTupel stepSize) indexVector

makeRGBTupel :: Double -> Int -> (Double,Double,Double)
makeRGBTupel stepSize modelNumber = (a,b,c)
  where  totalSize = (fromIntegral modelNumber) * stepSize 
         a = (rgbBoundries (totalSize  - 255))/255
	 b = (rgbBoundries (totalSize - a - 255))/255
	 c = (rgbBoundries (totalSize - a - b))/255

rgbBoundries :: Double -> Double	     
rgbBoundries rgbValue
  | rgbValue>210 = 210
  | rgbValue<50 = 50
  | otherwise = rgbValue        

text' :: String -> QDiagram Cairo V2 Double Any
text' t = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False 3 3) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate 0.75, negate 0.75))

textWithSize' :: String -> Double -> QDiagram Cairo V2 Double Any
textWithSize' t si = textSVG_ (TextOpts linLibertineFont INSIDE_H KERN False si si) t # fc black # fillRule EvenOdd # lw 0.0 # translate (r2 (negate siOffset, negate siOffset)) 
  where textLength = fromIntegral (length t) * 2
        siOffset = si/2
