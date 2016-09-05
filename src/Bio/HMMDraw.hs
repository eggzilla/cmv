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
import  GHC.Float
--import Bio.HMMData


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
   | modelDetail == "detailed" = hcat (map (drawHMMNodeVerbose alphabet "probability") currentnodes) <> background
   | otherwise = hcat (map drawHMMNodeSimple currentnodes)
     where nodenumber = fromIntegral $ length currentnodes
           currentnodes = HM.nodes model
           alphabet = (HM.alpha model)
           boxlength = 2 * (fromIntegral (length alphabet))
           background = rect (5 * nodenumber) (boxlength + 9)  # fc white # lw 0 # translate (r2 (((5 * nodenumber)/2 -2), (negate (boxlength + 7)/2 + 2)))
-- | 
--drawHMMNodeFlat :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeFlat node = rect 2 2 # lw 0.1  

-- | 
--drawHMMNodeSimple :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeSimple node =  rect 2 2 # lw 0.1

-- | 
--drawHMMNodeVerbose :: String -> String -> HM.HMMER3Node -> QDiagram b V2 n Any
drawHMMNodeVerbose alphabet emissiontype node = deletions === strutY 1 === insertions === strutY 1 === matches alphabet emissiontype node ||| strutX 1
deletions =  alignedText 0 0 "D" # translate (r2 (negate 0.25,negate 0.25)) <> circle 1 # lw 0.1 # fc white
insertions = alignedText 0 0 "I" # translate (r2 (negate 0.25,negate 0.25)) <> rect 2 2 # lw 0.1 # rotateBy (1/8) # fc white 


matches alphabet emissiontype node = entries # translate (r2 (negate 1, boxlength/4)) <> outerbox
  where outerbox = rect 4 boxlength # lw 0.1 # fc white
        entries = vcat (map (emissionEntry emissiontype) symbolsAndEmissions)
        symbolsAndEmissions = zip (map wrap alphabetSymbols) emissionEntries
        emissionEntries = setEmissions emissiontype (HM.matchEmissions node)
        boxlength = 2 * (fromIntegral (length alphabet))
        alphabetSymbols = HM.alphabetToSymbols alphabet

setEmissions :: String -> [Double] -> [Double]
setEmissions emissiontype emissions
  | emissiontype == "probability" = scoreentries
  | emissiontype == "score" = propentries
  | emissiontype == "bar" = barentries
    where scoreentries = emissions      
          propentries = map (exp . negate) emissions
          barentries = map (exp . negate) emissions

wrap x = [x]

         
--emissionEntry :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => String -> (String,Double) -> QDiagram b V2 n Any
emissionEntry emissiontype (symbol,emission) 
  | emissiontype == "probability" = textentry
  | emissiontype == "score" = textentry
  | emissiontype == "bar" = barentry
    where textentry = alignedText 0 0.1 (symbol ++ " " ++ printf "%.3f" emission) # translate (r2 (negate 0.5,0)) <> (rect 2 1 # lw 0 ) 
          --barentry =  stroke (textSVG symbol 2) ||| bar emission
          barentry = (alignedText 0 0.01  symbol <> (rect 2 1 # lw 0 ) )||| bar emission

--bar :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => Double -> QDiagram b V2 n Any
bar emission = (rect emission 1 # lw 0 # fc black # translate (r2 (negate (0.5 - (emission/2)),0)) <> rect 1 1 # lw 0.01 )


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
