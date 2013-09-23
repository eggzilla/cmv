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
drawCMGuideNode (number,label) =  text' label # translate (r2 (0,20)) <> text' number # translate (r2 (0,-20)) <> rect 100 100 # lw 1 # fc (labelToColor label)

-- | Render text as SVG
text' t = stroke (textSVG t 40) # fc black # fillRule EvenOdd

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
          
--print svg          
svgwidth = Just 100
svglength = Just 100            
svgsize = mkSizeSpec svgwidth svglength
printSVG = renderSVG "./testdiagram.svg" svgsize --testdrawing
