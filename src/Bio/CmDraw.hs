-- | Drawing of covariance model (http://www.tbi.univie.ac.at/software/cmcompare/) guide trees and highlighting comparison results
-- Drawing is done with the diagrams package

module CmDraw
    (
     drawCMGuideForest,
     drawCMGuideTrees,
     drawCMGuideTree,
     text',
     svgsize,
     printSVG,
    ) where
  
import Diagrams.Prelude
import Diagrams.TwoD
import Diagrams.Backend.SVG
import Data.Colour.SRGB
import Graphics.SVGFonts.ReadFont

-- | Draw one or more CM guide trees and concatenate them vertically
drawCMGuideForest cms = vcat' with { sep = 100 } (drawCMGuideTrees cms)

-- | Draw the Guide Trees of multiple CMs, utilizes drawCMGuideNode
--drawCMGuideTree  ->
drawCMGuideTrees cms  = map drawCMGuideTree cms

-- | Draw the guide Tree of a single CM, utilizes drawCMGuideNode
--drawCMGuideTree  ->
drawCMGuideTree nodes = hcat (drawCMGuideNodes nodes)

-- | Draw the guide Tree of a single CM, utilizes drawCMGuideNode
--drawCMGuideNodes :: [(String,String)] ->  [Diagram b0 R2]               
drawCMGuideNodes nodes = map drawCMGuideNode nodes
                                       

-- | Draws the guide tree nodes of a CM
--drawCMGuideNode :: (String,String) ->  Diagram b0 R2
drawCMGuideNode (number,label) =  text' label # translate (r2 (0,2)) <> text' number # translate (r2 (0,-2)) <> rect 10 10 # lw 1 # fc (labelToColor label)

-- | Render text as SVG
text' t = stroke (textSVG t 4) # fc black # fillRule EvenOdd

-- | Transform covariance model node labels to colors
--labelToColor :: String -> Color  
labelToColor label
   | label == "MATP" = sRGB24 211 211 211 -- P
   | label == "MATL" = sRGB24 211 211 211 -- L
   | label == "MATR" = sRGB24 211 211 211 -- R
   | label == "BIF"  = sRGB24 255 069 064 -- B
   | label == "ROOT" = sRGB24 245 245 245 -- S
   | label == "BEGL" = sRGB24 211 211 211 -- S
   | label == "BEGR" = sRGB24 211 211 211 -- S 
   | label == "END"  = sRGB24 245 245 245 -- E
labelToColor _ = sRGB24 245 245 245
          
--scaling

-- | Width of the drawing is determinded by the model with the max node number
svgwidth :: [[(String,String)]] -> Maybe Double
svgwidth processedCMs = Just (fromIntegral (maximum (map length processedCMs)))

-- | Length of the diagram is determined by the number of compared models
svglength :: [[(String,String)]] -> Maybe Double
svglength processedCMs = Just (fromIntegral (length processedCMs) * 40)

-- | Specifies the size of the diagram          
svgsize processedCMs = mkSizeSpec (svgwidth processedCMs) (svglength processedCMs)

diagramName = "./testdiagram.svg"

-- | Print drawn diagram as svg, already curried with diagram name, svgsize and the drawing have to specified
printSVG = renderSVG diagramName --svgsize --testdrawing
