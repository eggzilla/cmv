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
      printSVG
    ) where
  
import Diagrams.Prelude
import Diagrams.Backend.SVG
--import Graphics.SVGFonts
import Data.Typeable.Internal
import qualified Bio.HMMParser as HM
import Text.Printf
import Prelude
import GHC.Float
import qualified Bio.StockholmData as S
import qualified Data.Text as T    
import Data.Maybe
import qualified Data.Vector as V

-- | 
--drawHMMMER3s :: forall b. Renderable (Path V2 Double) b => String -> [HM.HMMER3] -> QDiagram b V2 Double Any
drawHMMMER3s modelDetail entryNumberCutoff emissiontype maxWidth hmms alns
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
  | otherwise = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail entryNumberCutoff maxWidth emissiontype) zippedInput))
    where zippedInput = zip hmms alns
          --maxWidth = (400 :: Double)

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
           alphabet = (HM.alpha model)
           alphabetSymbols = HM.alphabetToSymbols alphabet           
           boxlength = (fromIntegral (length alphabetSymbols)) + 1
	   nodeWidth = (6.0 :: Double)
	   nodeNumberPerRow = floor (maxWidth / nodeWidth - 2)
	   nodesIntervals = makeNodeIntervals nodeNumberPerRow nodeNumber
           verboseNodes = vcat' with { _sep = 2 } (V.toList (V.map (drawDetailedNodeRow alphabetSymbols emissiontype boxlength currentNodes) nodesIntervals))
           verboseNodesAlignment = alignTL (vcat' with { _sep = 5 }  [verboseNodes,alignmentDiagram])
           alignmentDiagram = if isJust aln then drawStockholm entriesNumberCutoff (fromJust aln) else mempty
           --connectedNodes = makeConnections boxlength currentNodes
           --selfconnectedNodes = makeSelfConnections boxlength currentNodes
           --arrowList = map makeArrow connectedNodes ++ map makeSelfArrow selfconnectedNodes
           --labelList = map makeLabel connectedNodes ++ map makeSelfLabel selfconnectedNodes

makeNodeIntervals :: Int -> Int  -> V.Vector (Int,Int)
makeNodeIntervals nodeNumberPerRow nodeNumber = rowIntervals
  where rowVector = V.iterateN rowNumber (1+) 0 
        rowNumber = ceiling $ (fromIntegral nodeNumber) / (fromIntegral nodeNumberPerRow)
	rowIntervals = V.map (setRowInterval nodeNumberPerRow nodeNumber) rowVector

setRowInterval nodeNumberPerRow nodeNumber index = (start,safeLength)
  where start = index*nodeNumberPerRow
        length = nodeNumberPerRow -1
	safeLength = if start +length > nodeNumber then nodeNumber - start -1 else length

drawDetailedNodeRow alphabetSymbols emissiontype boxlength allNodes (currentIndex,nextIndex) = detailedRow
  where currentNodes = V.slice currentIndex nextIndex allNodes
        detailedRow = applyAll (arrowList ++ labelList) detailedNodes  
        detailedNodes = hcat (V.toList (V.map (drawHMMNodeVerbose alphabetSymbols "box" boxlength) currentNodes))
        connectedNodes = makeConnections boxlength currentNodes
        selfConnectedNodes = makeSelfConnections boxlength currentNodes
        arrowList = V.toList (V.map makeArrow connectedNodes V.++ V.map makeSelfArrow selfConnectedNodes)
        labelList = V.toList (V.map makeLabel connectedNodes V.++ V.map makeSelfLabel selfConnectedNodes)

--drawStockholm                       
drawStockholm entriesNumberCutoff aln = alignTL (vcat' with { _sep = 1 } (map (drawStockholmEntry maxIdLength) currentEntries))
  where currentEntries = take entriesNumberCutoff (S.sequenceEntries aln)
        entryNumber = length currentEntries
        maxIdLength = maximum (map (T.length . S.sequenceId) currentEntries)

drawStockholmEntry maxIdLength entry = entryDia
  where entryText = T.unpack (seqId `T.append` spacer `T.append` (S.entrySequence entry))         
        seqId = S.sequenceId entry             
        spacerLength = (maxIdLength + 3) - T.length seqId 
        spacer = T.replicate spacerLength (T.pack " ")
        entryDia = hcat (map setLetter entryText)         
       
setLetter echar = alignedText 0.5 0.5 [echar] # fontSize 2 <> rect 0.5 1 # lw 0 -- # translate (r2 (negate 0.5, 0))                                           
setLabelLetter echar = alignedText 0.5 0.5 [echar] # fontSize 0.75 <> rect 0.4 0.5 # lw 0

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
makedd1A currentNode = (show ((HM.nodeId) currentNode) ++ "d", show ((HM.nodeId currentNode) + 1) ++ "d", maybe 0 ((roundPos 2) . exp . negate) (HM.d2d currentNode),(0,0.5))

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
--drawHMMNodeVerbose :: String -> String -> HM.HMMER3Node -> QDiagram b V2 n Any
drawHMMNodeVerbose alphabetSymbols emissiontype boxlength node
  | idNumber == 0 = idBox nid === strutY 0.5 === emptyDeletions nid === strutY 1.5 === insertions nid === strutY 1.5 === beginBox boxlength nid ||| strutX 4  
  | otherwise = idBox nid === strutY 0.5 === deletions nid === strutY 1.5 === insertions nid  === strutY 1.5 === matches alphabetSymbols emissiontype boxlength node ||| strutX 4
  where idNumber = HM.nodeId node
        nid = show idNumber

idBox nid = alignedText 0 0 nid # fontSize 2 # translate (r2 ((negate ((fromIntegral (length nid))/2)),0)) <> rect 1.5 1.5 # lw 0
              
deletions nid =  alignedText 0 0 "D" # translate (r2 (negate 0.25,0.25)) <> circle 3 # lw 0.1 # fc white # named (nid ++ "d")
emptyDeletions nid = circle 3 # lw 0.0 # fc white # named (nid ++ "d")
insertions nid = alignedText 0 0 "I" # translate (r2 (0,0.25)) <> rect 4.2426 4.2426 # lw 0.1 # rotateBy (1/8) # fc white # named (nid ++ "i")

matches alphabetSymbols emissiontype boxlength node = entries # translate (r2 (negate 2.5,boxlength/2 -1)) <> outerbox # named (nid ++ "m")
  where outerbox = rect 6 boxlength # lw 0.1 # fc white
        entries = vcat (map (emissionEntry emissiontype) symbolsAndEmissions)
        symbolsAndEmissions = zip (map wrap alphabetSymbols) (V.toList emissionEntries)
        emissionEntries = setEmissions emissiontype (HM.matchEmissions node)
        nid = show $ HM.nodeId node
	
beginBox boxlength nid = outerbox # named (nid ++ "m") <> alignedText 0.5 0.5 "BEGIN"
  where outerbox = rect 6 boxlength # lw 0.1 # fc white

transitions boxlength node = rect 6 height # lw 0 # translate (r2(0,negate (height/2)))
  where height = (boxlength + 1 + 1 + 6 + 6)

setEmissions :: String -> V.Vector Double -> V.Vector Double
setEmissions emissiontype emissions
  | emissiontype == "probability" = scoreentries
  | emissiontype == "score" = propentries
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


-- | 
--drawHMMNodeVerbose :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => HM.HMMER3Node -> QDiagram b V2 n Any
--drawHMMNodeVerbose node =  text' label # translate (r2 (0,2)) <> text' number # translate (r2 (0,-2)) <> rect 10 10 # lw 0.5 # fc (labelToColor label)


-- | Render text as SVG
--text' :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => String -> QDiagram b V2 n Any
--text' t = stroke (textSVG t 1) # fc black # fillRule EvenOdd # lw 0.1
          
--scaling
-- | Specifies the size of the diagram. Absolute adapts to overall size according to subdiagrams
--svgsize processedCMs = mkSizeSpec (svgwidth processedCMs) (svglength processedCMs)
 --svgsize = Absolute
svgsize :: SizeSpec V2 Double
svgsize = mkSizeSpec2D Nothing Nothing

diagramName :: [Char]
diagramName = "./diagram.svg"

-- | Print drawn diagram as svg, already curried with diagram name, svgsize and the drawing have to specified
printSVG :: forall n. (RealFloat n, Show n, Data.Typeable.Internal.Typeable n) => SizeSpec V2 n -> QDiagram SVG V2 n Any -> IO ()
printSVG = renderSVG diagramName 

roundPos :: (RealFrac a) => Int -> a -> a
roundPos positions number  = (fromInteger $ round $ number * (10^positions)) / (10.0^^positions)
 
