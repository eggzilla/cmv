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
   | modelDetail == "detailed" = hcat (map (drawHMMNodeVerbose alphabetSymbols "box" boxlength) currentnodes) # bg white 
   | otherwise = hcat (map drawHMMNodeSimple currentnodes)
     where nodenumber = fromIntegral $ length currentnodes
           currentnodes = HM.nodes model
           alphabet = (HM.alpha model)
           alphabetSymbols = HM.alphabetToSymbols alphabet           
           boxlength = (fromIntegral (length alphabetSymbols)) + 1
                       
-- | 
--drawHMMNodeFlat :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeFlat node = rect 2 2 # lw 0.1  

-- | 
--drawHMMNodeSimple :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeSimple node =  rect 2 2 # lw 0.1

-- | 
--drawHMMNodeVerbose :: String -> String -> HM.HMMER3Node -> QDiagram b V2 n Any
drawHMMNodeVerbose alphabetSymbols emissiontype boxlength node = deletions === strutY 1 === insertions === strutY 1 === matches alphabetSymbols emissiontype boxlength node ||| strutX 1
deletions =  alignedText 0 0 "D" # translate (r2 (0.25,0.25)) <> circle 2 # lw 0.1 # fc white
insertions = alignedText 0 0 "I" # translate (r2 (0.25,0.25)) <> rect 4 4 # lw 0.1 # rotateBy (1/8) # fc white 

matches alphabetSymbols emissiontype boxlength node = entries # translate (r2 (negate 2.5,boxlength/2 -1)) <> outerbox
  where outerbox = rect 6 boxlength # lw 0.1 # fc white
        entries = vcat (map (emissionEntry emissiontype) symbolsAndEmissions)
        symbolsAndEmissions = zip (map wrap alphabetSymbols) emissionEntries
        emissionEntries = setEmissions emissiontype (HM.matchEmissions node)

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
          barentry = (alignedText 0 0.01 symbol  # translate (r2 (negate 0.25,0.5)) <> (rect 2 1 # lw 0 )) ||| bar emission

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
