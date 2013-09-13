module CmDraw
    (
     printSVG
    ) where
  
import Diagrams.Prelude
import Diagrams.TwoD
import Diagrams.Backend.SVG

drawSVG = circle 1

--print svg          
svgwidth = Just 100
svglength = Just 100            
svgsize = mkSizeSpec svgwidth svglength                            
printSVG = renderSVG "./testdiagram.svg" svgsize drawSVG

