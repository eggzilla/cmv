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
import qualified Bio.HMMData as HM
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
   | modelDetail == "detailed" = hcat (map drawHMMNodeVerbose (HM.nodes model))
   | otherwise = hcat (map drawHMMNodeSimple (HM.nodes model))

-- | 
drawHMMNodeFlat :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeFlat node =  rect 2 2 # lw 0.1 

-- | 
drawHMMNodeSimple :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeSimple node =  rect 2 2 # lw 0.1

-- | 
drawHMMNodeVerbose :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMNodeVerbose node =  rect 2 2 # lw 0.1 

-- | 
--drawHMMNodeVerbose :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => HM.HMMER3Node -> QDiagram b V2 n Any
--drawHMMNodeVerbose node =  text' label # translate (r2 (0,2)) <> text' number # translate (r2 (0,-2)) <> rect 10 10 # lw 0.5 # fc (labelToColor label)


-- | Render text as SVG
text' :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => String -> QDiagram b V2 n Any
text' t = stroke (textSVG t 4) # fc black # fillRule EvenOdd


          
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

