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
--import Bio.HMMData


-- | 
drawHMMMER3s :: forall b. Renderable (Path V2 Double) b => String -> [HM.HMMER3] -> QDiagram b V2 Double Any
drawHMMMER3s modelDetail hmms
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail) hmms))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 8 } (map (drawHMMER3 modelDetail) hmms))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail) hmms))
  | otherwise = alignTL (vcat' with { _sep = 40 } (map (drawHMMER3 modelDetail) hmms))

-- |
drawHMMER3 :: forall n b. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => String -> HM.HMMER3 -> QDiagram b V2 n Any
drawHMMER3 modelDetail model
   | modelDetail == "flat" = hcat (map drawHMMNodeFlat (HM.nodes model))
   | modelDetail == "simple" = hcat (map drawHMMNodeSimple (HM.nodes model))
   | modelDetail == "detailed" = hcat (map (drawHMMNodeVerbose (HM.alpha model)) (HM.nodes model))
   | otherwise = hcat (map drawHMMNodeSimple (HM.nodes model))

-- | 
drawHMMNodeFlat :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeFlat node = rect 2 2 # lw 0.1  

-- | 
drawHMMNodeSimple :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeSimple node =  rect 2 2 # lw 0.1

-- | 
--drawHMMNodeVerbose :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeVerbose alphabet node = deletions === strutY 1 === insertions === strutY 1 === (matches alphabet node) ||| strutX 1
deletions = circle 1 # lw 0.1
insertions = rect 2 2 # lw 0.1 # rotateBy (1/8)
--matches = rect 2 2 # lw 0.1
matches alphabet node = alignL (outerbox <> entries)
  where outerbox = rect 4 boxlength # lw 0.1
        entries = vcat (map emissionEntry (zip (map wrap alphabetSymbols) entrytype))
        entrytype = propentries   
        scoreentries = (HM.matchEmissions node)          
        propentries = map (exp . negate) (HM.matchEmissions node)
        barentries = map (exp . negate) (HM.matchEmissions node)
        boxlength = 2 * (fromIntegral (length alphabet))
        alphabetSymbols = HM.alphabetToSymbols alphabet

wrap x = [x]

--emissionEntry ::         
emissionEntry (symbol,emission) = text' (symbol ++ " " ++ show emission)

-- | 
--drawHMMNodeVerbose :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => HM.HMMER3Node -> QDiagram b V2 n Any
--drawHMMNodeVerbose node =  text' label # translate (r2 (0,2)) <> text' number # translate (r2 (0,-2)) <> rect 10 10 # lw 0.5 # fc (labelToColor label)


-- | Render text as SVG
text' :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => String -> QDiagram b V2 n Any
text' t = stroke (textSVG t 1) # fc black # fillRule EvenOdd # lw 0.1
          
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

