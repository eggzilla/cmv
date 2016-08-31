-- | 
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes #-}

module Bio.HMMDraw
    (
      drawHMMs,
      drawHMM
  
    ) where
  
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.SVGFonts
import Data.Typeable.Internal
import qualified Bio.HMMData as HM
import Data.List
import Text.Parsec.Error
import qualified Data.Text as T
import qualified Data.Vector as V

-- | 
drawHMMs :: forall b. Renderable (Path V2 Double) b => [Char] -> [HM.HMMER3] -> QDiagram b V2 Double Any
drawHMMs modelDetail cms
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (map (drawHMM modelDetail) hmms))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 8 } (map (drawHMM modelDetail) hmms))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (map (drawHMM modelDetail) hmms))
  | otherwise = alignTL (vcat' with { _sep = 40 } drawHMM modelDetail hmms))

-- |
drawHMM :: forall n b. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => [Char] -> [HM.HMMER3] -> QDiagram b V2 n Any
drawHMM modelDetail model
   | modelDetail == "flat" = hcat (map drawHMMNodesFlat (HM.nodes model))
   | modelDetail == "simple" = hcat (map drawHMMNodesSimple (HM.nodes model))
   | modelDetail == "detailed" = hcat (map drawHMMNodesVerbose (HM.nodes model))
   | otherwise = hcat (map drawHMMNodesSimple nodes)

-- | 
draHMMNodeFlat :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => HM.HMMER3Node -> b
drawHMMFlat node =  rect 2 2 # lw 0.1 

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

