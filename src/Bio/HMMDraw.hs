-- | 
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes #-}

module Bio.HMMDraw
    (
      drawHMMMER3s,
      drawHMMER3,
      svgsize,
      diagramName,
      printHMM,
      getComparisonsHighlightParameters,
      drawHMMComparison
    ) where
  
import Diagrams.Prelude
import qualified Diagrams.Backend.Cairo as C
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
import Data.Colour.SRGB.Linear

drawHMMComparison modelDetail entryNumberCutoff emissiontype maxWidth hmms alns comparisons
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | otherwise = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
    where zippedInput = zip3 (hmms alns comparisonNodeLabels)
          colorVector = makeColorVector modelNumber 
	  modelNumber = length hmms
	  modelNames = V.map HM.name hmms
	  nameColorVector = V.zipWith (\a b -> (a,b)) modelNames colorVector
	  comparisonNodeLabels = map (getComparisonNodeLabels comparisons nameColorVector) models
	            
-- | 
--drawHMMMER3s :: forall b. Renderable (Path V2 Double) b => String -> [HM.HMMER3] -> QDiagram b V2 Double Any
drawHMMMER3s modelDetail entryNumberCutoff emissiontype maxWidth hmms alns
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | otherwise = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
    where zippedInput = zip hmms alns
          

-- |
--drawHMMER3 :: forall n b. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => String -> HM.HMMER3 -> QDiagram b V2 n Any
drawHMMER3 modelDetail entriesNumberCutoff maxWidth emissiontype (model,aln)
   | modelDetail == "flat" = hcat $ V.toList (V.map drawHMMNodeFlat currentNodes)
   | modelDetail == "simple" = hcat $ V.toList (V.map drawHMMNodeSimple currentNodes)
--   | modelDetail == "detailed" = applyAll ([bg white] ++ arrowList ++ labelList) verboseNodesAlignment
   | modelDetail == "detailed" = applyAll ([bg white]) verboseNodesAlignment
   | otherwise = hcat $ V.toList (V.map drawHMMNodeSimple currentNodes)
     where nodeNumber = fromIntegral $ length currentNodes
           currentNodes = HM.begin model `V.cons` HM.nodes model
           alphabet = HM.alpha model
           alphabetSymbols = HM.alphabetToSymbols alphabet           
           boxlength = (fromIntegral (length alphabetSymbols)) + 1
           nodeWidth = (6.0 :: Double)
           nodeNumberPerRow = floor (maxWidth / nodeWidth - 2)
           nodesIntervals = makeNodeIntervals nodeNumberPerRow nodeNumber
           verboseNodes = vcat' with { _sep = 2 } (V.toList (V.map (drawDetailedNodeRow alphabetSymbols emissiontype boxlength nodeNumber currentNodes) nodesIntervals))
           verboseNodesAlignment = alignTL (vcat' with { _sep = 5 }  [verboseNodes,alignmentDiagram])
           alignmentDiagram = if isJust aln then drawStockholmLines entriesNumberCutoff maxWidth (fromJust aln) else mempty
           --connectedNodes = makeConnections boxlength currentNodes
           --selfconnectedNodes = makeSelfConnections boxlength currentNodes
           --arrowList = map makeArrow connectedNodes ++ map makeSelfArrow selfconnectedNodes
           --labelList = map makeLabel connectedNodes ++ map makeSelfLabel selfconnectedNodes

makeNodeIntervals :: Int -> Int -> V.Vector (Int,Int)
makeNodeIntervals nodeNumberPerRow nodeNumber = rowIntervals
  where rowVector = V.iterateN rowNumber (1+) 0 
        rowNumber = ceiling $ (fromIntegral nodeNumber) / (fromIntegral nodeNumberPerRow)
	rowIntervals = V.map (setRowInterval nodeNumberPerRow nodeNumber) rowVector

setRowInterval nodeNumberPerRow nodeNumber index = (start,safeLength)
  where start = index*nodeNumberPerRow
        length = nodeNumberPerRow 
	safeLength = if start +length > nodeNumber then (nodeNumber - start) else length

drawDetailedNodeRow alphabetSymbols emissiontype boxlength lastIndex allNodes (currentIndex,nodeSliceLength) = detailedRow
  where currentNodes = V.slice currentIndex nodeSliceLength allNodes
        withLastRowNodes = V.slice (currentIndex -1) (nodeSliceLength +1) allNodes
        detailedRow = applyAll (lastRowList ++ arrowList ++ labelList) detailedNodes
	nextIndex = currentIndex + nodeSliceLength
        detailedNodes = hcat (V.toList (V.map (drawHMMNodeVerbose alphabetSymbols emissiontype boxlength currentIndex nextIndex lastIndex) currentNodes))
        arrowNodes = if currentIndex > 1 then currentNodes else currentNodes
        connectedNodes = makeConnections boxlength arrowNodes
        selfConnectedNodes = makeSelfConnections boxlength arrowNodes
        arrowList = V.toList (V.map makeArrow connectedNodes V.++ V.map makeSelfArrow selfConnectedNodes)
        labelList = V.toList (V.map makeLabel connectedNodes V.++ V.map makeSelfLabel selfConnectedNodes)
        --add arrows and labels for transitions from previous row
        lastNode = if currentIndex > 1 then V.slice (currentIndex -1) 1 allNodes else V.empty
        lastRowConnections = makeLastRowConnections boxlength lastNode
        lastRowList = V.toList (V.map makeArrow lastRowConnections V.++ V.map makeLabel lastRowConnections)   
       
setLetter echar = alignedText 0.5 0.5 [echar] # fontSize 2 <> rect 1.0 1.0 # lw 0 -- # translate (r2 (negate 0.5, 0))
setLabelLetter echar = alignedText 0.5 0.5 [echar] # fontSize 0.75 <> rect 0.4 0.5 # lw 0

makeLastRowConnections boxlength currentnodes =  mm1A V.++ md1A V.++ im1A V.++ dm1A V.++ dd1A
  where mm1A = V.map makemm1A currentnodes 
        md1A = V.map (makemd1A boxlength) currentnodes
        im1A = V.map makeim1A currentnodes
        dm1A = V.map (makedm1A boxlength) currentnodes
        dd1A = V.map makedd1A currentnodes

makeConnections boxlength currentnodes =  mm1A V.++ miA V.++ md1A V.++ im1A V.++ dm1A V.++ dd1A
  where mm1A = V.map makemm1A currentnodes 
        miA = V.map (makemiA boxlength) currentnodes
        md1A = V.map (makemd1A boxlength) currentnodes
        im1A = V.map makeim1A currentnodes
        dm1A = V.map (makedm1A boxlength) currentnodes
        dd1A = V.map makedd1A currentnodes

makeSelfConnections boxlength currentnodes = V.map (makeiiA boxlength) currentnodes

makemm1A currentNode = (show ((HM.nodeId) currentNode) ++ "m", show ((HM.nodeId currentNode) + 1) ++ "m", maybe 0 ((roundPos 2) . exp . negate) (HM.m2m currentNode),(0,0.5)) 
makemiA boxlength currentNode = (show ((HM.nodeId) currentNode) ++ "m", show ((HM.nodeId currentNode)) ++ "i",  maybe 0 ((roundPos 2) . exp . negate) (HM.m2i currentNode),(0,setiayOffset boxlength))
makemd1A boxlength currentNode = (show ((HM.nodeId) currentNode) ++ "m", show ((HM.nodeId currentNode) + 1) ++ "d", maybe 0 ((roundPos 2) . exp . negate) (HM.m2d currentNode),(1.5,2.0))
makeim1A currentNode = (show ((HM.nodeId) currentNode) ++ "i", show ((HM.nodeId currentNode) + 1) ++ "m", maybe 0 ((roundPos 2) . exp . negate) (HM.i2m currentNode),(0,negate 0.5))
makeiiA boxlength currentNode = (show ((HM.nodeId) currentNode) ++ "i", show ((HM.nodeId currentNode)) ++ "i", maybe 0 ((roundPos 2) . exp . negate) (HM.i2i currentNode),(0,3.0))
makedm1A boxlength currentNode = (show ((HM.nodeId) currentNode) ++ "d", show ((HM.nodeId currentNode) + 1) ++ "m", maybe 0 ((roundPos 2) . exp . negate) (HM.d2m currentNode),(negate 1.5,2.0))
makedd1A currentNode = (show ((HM.nodeId) currentNode) ++ "d", show ((HM.nodeId currentNode) + 1) ++ "d", maybe 0 ((roundPos 2) . exp . negate) (HM.d2d currentNode),(negate 1,1))

setiayOffset boxlength
  | boxlength <= 10 = negate 0.3
  | otherwise = 3.5

makeArrow (lab1,lab2,weight,_) = connectOutside' arrowStyle1 lab1 lab2 
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
      atop (position [((midpoint # translateX (negate 0.25 + xOffset) # translateY (0 + yOffset)), (hcat (map setLabelLetter (show weight))) )])

makeSelfLabel (n1,n2,weight,(xOffset,yOffset))=
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v ^/ 2)
    in
      atop (position [(midpoint # translateX (negate 0.25 + xOffset) # translateY (0 + yOffset), (hcat (map setLabelLetter (show weight))))])

-- | 
--drawHMMNodeFlat :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeFlat node = rect 2 2 # lw 0.1  

-- | 
--drawHMMNodeSimple :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeSimple node = rect 2 2 # lw 0.1

-- | 
--drawHMMNodeVerbose :: String -> String -> Int -> Int -> Int -> HM.HMMER3Node -> QDiagram b V2 n Any
drawHMMNodeVerbose alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex node 
  | idNumber == 0 = beginBox
  | idNumber == (lastIndex - 1) = nodeBox ||| endBox
  | idNumber == rowStart = rowStartBox idNumber boxlength ||| nodeBox
  | idNumber == rowEnd - 1 = nodeBox ||| rowEndBox idNumber boxlength
  | otherwise = nodeBox 
  where idNumber = HM.nodeId node
        nid = show idNumber
        beginBox = rect 1 1 # lw 0.0 ||| (idBox nid === strutY 0.5 === emptyDeletions === strutY 1.5 === insertions nid === strutY 1.5 === beginState boxlength nid) ||| strutX 4
	nodeBox = idBox nid === strutY 0.5 === deletions nid === strutY 1.5 === insertions nid  === strutY 1.5 === matches alphabetSymbols emissiontype boxlength node ||| strutX 4
	endBox = emptyIdBox === strutY 0.5 === emptyDeletions === strutY 1.5 === emptyInsertions  === strutY 1.5 === endState boxlength idNumber ||| strutX 4

-- | idBox associates the node with its index and a tupel of  a list of modelidentifiers and the total model number
idBox nid labelList= alignedText 0 0 nid # fontSize 2 # translate (r2 ((negate ((fromIntegral (length nid))/2)), negate 1.25)) <> rect 1.5 3 # lw 0.1
emptyIdBox = rect 1.5 1.5 # lw 0
rowStartBox idNumber boxlength = rect 0.1 1.5 #lw 0.0 === rect 0.1 6 # lw 0.1 # named (nid ++ "d") === rect 0.1 6 # lw 0.1 #named (nid ++ "i") === rect 0.1 (boxlength + 2) # lw 0.1 # named (nid ++ "m") ||| strutX 2.5
  where nid = show (idNumber - 1)
rowEndBox idNumber boxlength = rect 0.1 1.5 #lw 0.0 === rect 0.1 6 # lw 0.1 #named (nid ++ "d") === rect 0.1 6 # lw 0.1 #named (nid ++ "i") === rect 0.1 (boxlength + 2) #lw 0.1 #named (nid ++ "m")
  where nid = show (idNumber + 1)
deletions nid =  alignedText 0 0 "D" # translate (r2 (negate 0.25,0.25)) <> circle 3 # lw 0.1 # fc white # named (nid ++ "d")
emptyDeletions = circle 3 # lw 0.0 # fc white 
insertions nid = alignedText 0 0 "I" # translate (r2 (0,0.25)) <> rect 4.2426 4.2426 # lw 0.1 # rotateBy (1/8) # fc white # named (nid ++ "i")
emptyInsertions = rect 4.2426 4.2426 # lw 0 # rotateBy (1/8) # fc white 
matches alphabetSymbols emissiontype boxlength node = entries # translate (r2 (negate 2.5,boxlength/2 -1)) <> outerbox # named (nid ++ "m")
  where outerbox = rect 6 boxlength # lw 0.1 # fc white
        entries = vcat (map (emissionEntry emissiontype) symbolsAndEmissions)
        symbolsAndEmissions = zip (map wrap alphabetSymbols) (V.toList emissionEntries)
        emissionEntries = setEmissions emissiontype (HM.matchEmissions node)
        nid = show $ HM.nodeId node

-- B → M 1 , B → I 0 , B → D 1 ; I 0 → M 1 , I 0 → I 0
beginState boxlength nid = alignedText 0.5 0.5 "BEGIN" <> outerbox # named (nid ++ "m") <> rect 6 boxlength # named (nid ++ "d")
  where outerbox = rect 6 boxlength # lw 0.1 # fc white

endState boxlength idNumber = alignedText 0.5 0.5 "END" <> outerbox # named (nid ++ "m") <> rect 6 boxlength # named (nid ++ "d") <> rect 6 boxlength # named (nid ++ "i")
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

printHMM outputName = C.renderCairo outputName

roundPos :: (RealFrac a) => Int -> a -> a
roundPos positions number  = (fromInteger $ round $ number * (10^positions)) / (10.0^^positions)

getComparisonNodeLabels :: [HMMCompareResult] -> V.Vector Color -> HM.HMMER3 -> [[Color,V.Vector]]
getComparisonNodeLabels comparsionResults colorVector model =
   where modelName = HM.name model 
         relevantComparisons1 = (filter ((modelName==) . name1)) comparsionResults)
	 modelcolorNodeInterval1 = relevantComparisons1
	 relevantComparisons2 = (filter ((modelName==) . name2)) comparsionResults)
	 

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

-- | Colors from color list are selected according to in which of the compared trees the node is contained.
--selectColors :: [Int] -> [GVA.Color] -> GVAC.ColorList
--selectColors inTrees currentColorList = GVAC.toColorList (map (\i -> currentColorList !! i) inTrees)

-- | A color list is sampled from the spectrum according to how many trees are compared.
--makeColorList :: Int -> [GVA.Color]
--makeColorList modelNumber = cList
--  where cList = map (\i -> GVAC.HSV ((fromIntegral i/fromIntegral neededColors) * 0.708) 0.5 1.0) [0..neededColors]
--        neededColors = treeNumber - 1

makeColorVector modelNumber = V.map rgb modelRGBTupel
   where indexVector = V.iterateN modelNumber (1+) 1
         stepSize = (765 :: Double) / modelNumber
	 modelRGBTupel = V.map makeRGBTupel stepSize indexVector

makeRGBTupel stepSize modelNumber = (a,b,c)
  where  totalSize = modelNumber * stepSize 
         a = RGBboundries (totalSize - 255)
	 b = RGBboundries (totalsize - a - 255)
	 c = RGBboundries (totalsize - a - b)
	 