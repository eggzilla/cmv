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
drawCMGuideForest cms = alignTL (vcat' with { sep = 30 } (drawCMGuideTrees cms)) <> highlightComparison 1 5 2 10 <> highlightComparison 1 50 2 100

-- | Highlight comparison by connecting the delimiting nodes of the aligned nodes of both models
-- takes the model identifier of both models and the starting and ending nodes of both models as arguments.
 
highlightComparison model1index node1index model2index node2index = connection (getNodeCoordinates model1index node1index) (getNodeCoordinates model2index node2index)

-- | returns the center coordinates for an Covariance model guide tree node
getNodeCoordinates :: Int -> Int -> P2
getNodeCoordinates modelindex nodeindex = p2 (fromIntegral x, fromIntegral y)
   where y = (5 + (40 * (modelindex - 1))) * (-1)
         x = (5 + (10 * (nodeindex - 1 )))

--connection1 :: Int-> Int -> Int -> Int -> 
connection a b = fromVertices [a,b] # lw 4 # lc red


-- polygon with { polyType = PolySides [13,23,43,53] [5,3,5,2,53,3,34,34], polyOrient = OrientV }
--fromVertices (map p2 [(-5,5),(0,-20)]) # lw 1 <> fromVertices (map p2 [(5,-5),(0,-20)]) # lw 1
--(-5,5)
-- alignX 0 (alignY 0 ((rect 1 1) # fc black)) 

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
drawCMGuideNode (number,label) =  text' label # translate (r2 (0,2)) <> text' number # translate (r2 (0,-2)) <> rect 10 10 # lw 0.5 # fc (labelToColor label)

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
svgwidth processedCMs = Just (fromIntegral 1) --Just (fromIntegral (maximum (map length processedCMs)))

-- | Length of the diagram is determined by the number of compared models
svglength :: [[(String,String)]] -> Maybe Double
svglength processedCMs = Just (fromIntegral (length processedCMs))

-- | Specifies the size of the diagram          
--svgsize processedCMs = mkSizeSpec (svgwidth processedCMs) (svglength processedCMs)
svgsize = Absolute
diagramName = "./testdiagram.svg"

-- | Print drawn diagram as svg, already curried with diagram name, svgsize and the drawing have to specified
printSVG = renderSVG diagramName --svgsize --testdrawing
