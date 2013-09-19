module CmDraw
    (
     printSVG
    ) where
  
import Diagrams.Prelude
import Diagrams.TwoD
import Diagrams.Backend.SVG
import Graphics.SVGFonts.ReadFont

-- | Draw the guide Tree of a single CM, utilizes drawCMGuideNode
--drawCMGuideTree :: CM -> 

-- | Draws the guide tree nodes of a CM
drawCMGuideNode label number =  text' label # translate (r2 (0,20)) <> text' number # translate (r2 (0,-20)) <> rect 100 100 # lw 1 # fc lightgrey
text' t = stroke (textSVG t 40) # fc black # fillRule EvenOdd

--print svg          
svgwidth = Just 100
svglength = Just 100            
svgsize = mkSizeSpec svgwidth svglength
testdrawing = drawCMGuideNode "test" "1" ||| drawCMGuideNode "test" "2"||| drawCMGuideNode "test" "3"||| drawCMGuideNode "test" "4"  
printSVG = renderSVG "./testdiagram.svg" svgsize testdrawing
