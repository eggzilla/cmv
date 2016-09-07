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
import Graphics.SVGFonts
import Data.Typeable.Internal
import qualified Bio.HMMParser as HM
import Text.Printf
import Prelude
import GHC.Float


-- | 
--drawHMMMER3s :: forall b. Renderable (Path V2 Double) b => String -> [HM.HMMER3] -> QDiagram b V2 Double Any
drawHMMMER3s modelDetail hmms
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail) hmms))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail) hmms))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail) hmms))
  | otherwise = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail) hmms))

-- |
--drawHMMER3 :: forall n b. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => String -> HM.HMMER3 -> QDiagram b V2 n Any
drawHMMER3 modelDetail model
   | modelDetail == "flat" = hcat (map drawHMMNodeFlat currentnodes)
   | modelDetail == "simple" = hcat (map drawHMMNodeSimple currentnodes)
   | modelDetail == "detailed" = applyAll ([bg white] ++ arrowList) verboseNodes
   | otherwise = hcat (map drawHMMNodeSimple currentnodes)
     where nodenumber = fromIntegral $ length currentnodes
           currentnodes = HM.nodes model
           alphabet = (HM.alpha model)
           alphabetSymbols = HM.alphabetToSymbols alphabet           
           boxlength = (fromIntegral (length alphabetSymbols)) + 1
           verboseNodes =  hcat (map (drawHMMNodeVerbose alphabetSymbols "box" boxlength) currentnodes)
           arrowList = makeArrows currentnodes
           --arrowStyle1 = with & arrowHead .~ spike & shaftStyle %~ lw 0.01 & headLength .~ normalized 0.001
           
makeArrows currentnodes = (map makeArrow (mm1A ++ md1A ++ im1A ++ dm1A ++ dd1A)) ++ (map makeSelfArrow iiA)
  where mm1A = map makemm1A currentnodes 
        miA = map makemiA currentnodes
        md1A = map makemd1A currentnodes
        im1A = map makeim1A currentnodes
        iiA = map makeiiA currentnodes
        dm1A = map makedm1A currentnodes
        dd1A = map makedd1A currentnodes
makemm1A currentNode = (show ((HM.nodeId) currentNode) ++ "m", show ((HM.nodeId currentNode) + 1) ++ "m", maybe 0 (*0.01) (HM.m2m currentNode)) 
makemiA currentNode = (show ((HM.nodeId) currentNode) ++ "m", show ((HM.nodeId currentNode)) ++ "i", (HM.m2i currentNode))
makemd1A currentNode = (show ((HM.nodeId) currentNode) ++ "m", show ((HM.nodeId currentNode) + 1) ++ "d", maybe 0 (*0.01) (HM.m2d currentNode))
makeim1A currentNode = (show ((HM.nodeId) currentNode) ++ "i", show ((HM.nodeId currentNode) + 1) ++ "m", maybe 0 (*0.01) (HM.i2m currentNode))
makeiiA currentNode = (show ((HM.nodeId) currentNode) ++ "i", show ((HM.nodeId currentNode)) ++ "i", maybe 0 (*0.01) (HM.i2i currentNode))
makedm1A currentNode = (show ((HM.nodeId) currentNode) ++ "d", show ((HM.nodeId currentNode) + 1) ++ "m", maybe 0 (*0.01) (HM.d2m currentNode))
makedd1A currentNode = (show ((HM.nodeId) currentNode) ++ "d", show ((HM.nodeId currentNode) + 1) ++ "d", maybe 0 (*0.01) (HM.d2d currentNode))
              
makeArrow (lab1,lab2,weight) = connectOutside' arrowStyle1 lab1 lab2
  where arrowStyle1 = with & arrowHead .~ spike & shaftStyle %~ lw (local weight) & headLength .~ local 0.001
makeSelfArrow (lab1,lab2,weight) = connectPerim' arrowStyle lab1 lab1 (4/12 @@ turn) (2/12 @@ turn)
  where arrowStyle = with  & arrowHead .~ spike & arrowShaft .~ shaft' & arrowTail .~ lineTail & tailTexture .~ solid black  & shaftStyle %~ lw (local weight) & headLength .~ local 0.001  & tailLength .~ 1
        shaft' = arc xDir (-2.7/5 @@ turn)

    --arrowStyle = with  & arrowHead  .~ spike  & arrowShaft .~ line & shaftStyle %~ lw 0.01 & headLength .~ normalized 0.001
    --    line = trailFromOffsets [unitX]

-- | 
--drawHMMNodeFlat :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeFlat node = rect 2 2 # lw 0.1  

-- | 
--drawHMMNodeSimple :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeSimple node = rect 2 2 # lw 0.1

-- | 
--drawHMMNodeVerbose :: String -> String -> HM.HMMER3Node -> QDiagram b V2 n Any
drawHMMNodeVerbose alphabetSymbols emissiontype boxlength node = deletions nid === strutY 1.5 === insertions nid  === strutY 1.5 === matches alphabetSymbols emissiontype boxlength node ||| strutX 4--- transitions boxlength node
  where nid = show $ HM.nodeId node

deletions nid =  alignedText 0 0 "D" # translate (r2 (negate 0.25,0.25)) <> circle 3 # lw 0.1 # fc white # named (nid ++ "d")

insertions nid = alignedText 0 0 "I" # translate (r2 (0,0.25)) <> rect 4.2426 4.2426 # lw 0.1 # rotateBy (1/8) # fc white # named (nid ++ "i")

matches alphabetSymbols emissiontype boxlength node = entries # translate (r2 (negate 2.5,boxlength/2 -1)) <> outerbox # named (nid ++ "m")
  where outerbox = rect 6 boxlength # lw 0.1 # fc white
        entries = vcat (map (emissionEntry emissiontype) symbolsAndEmissions)
        symbolsAndEmissions = zip (map wrap alphabetSymbols) emissionEntries
        emissionEntries = setEmissions emissiontype (HM.matchEmissions node)
        nid = show $ HM.nodeId node

transitions boxlength node = rect 6 height # lw 0 # translate (r2(0,negate (height/2)))
  where height = (boxlength + 1 + 1 + 6 + 6)

setEmissions :: String -> [Double] -> [Double]
setEmissions emissiontype emissions
  | emissiontype == "probability" = scoreentries
  | emissiontype == "score" = propentries
  | emissiontype == "bar" = barentries
  | otherwise = barentries
    where scoreentries = emissions      
          propentries = map (exp . negate) emissions
          barentries = map (exp . negate) emissions

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
 
