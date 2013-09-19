module CmDraw
    (
     printSVG
    ) where
  
import Diagrams.Prelude
import Diagrams.TwoD
import Diagrams.Backend.SVG
import Data.Colour.SRGB
import Graphics.SVGFonts.ReadFont

-- | Draw the guide Tree of a single CM, utilizes drawCMGuideNode
--drawCMGuideTree :: CM -> 

-- | Draws the guide tree nodes of a CM
drawCMGuideNode label number =  text' label # translate (r2 (0,20)) <> text' number # translate (r2 (0,-20)) <> rect 100 100 # lw 1 # fc (labelToColor label)
text' t = stroke (textSVG t 40) # fc black # fillRule EvenOdd

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
testdrawing = drawCMGuideNode "ROOT" "1" ||| drawCMGuideNode "BIF" "2"||| drawCMGuideNode "BEGL" "3"||| drawCMGuideNode "END" "4"  
printSVG = renderSVG "./testdiagram.svg" svgsize testdrawing
