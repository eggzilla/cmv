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

--ComparisonBoundry = data ComparisonBoundry{
                      

    
-- | Draw one or more CM guide trees and concatenate them vertically
drawCMGuideForest cms = alignTL (vcat' with { sep = 40 } (drawCMGuideTrees cms)) <> highlightComparisonTrail 1 5 2 10 1 50 2 80 

                        
-- | Highlight comparison by connecting the delimiting nodes of the aligned nodes of both models
-- takes the model identifier of both models and the starting and ending nodes of both models as arguments.
highlightComparisonLines a b c d e f g h = highlightComparisonIntervalBoundry a b c d <> highlightComparisonIntervalBoundry e f g h
highlightComparisonTrail a b c d e f g h = connectionTrail (getNodeCoordinates a b) (getNodeCoordinates c d) (getNodeCoordinates e f) (getNodeCoordinates g h)
highlightComparisonIntervalBoundry model1index node1index model2index node2index = connectionLine (getNodeCoordinates model1index node1index) (getNodeCoordinates model2index node2index)

-- | returns the center coordinates for an Covariance model guide tree node
getNodeCoordinates :: Int -> Int -> P2
getNodeCoordinates modelindex nodeindex = p2 (fromIntegral x, fromIntegral y)
   where y = (10 + (45 * (modelindex - 1))) * (-1)
         x = (5 + (10 * (nodeindex - 1 )))

--connection1 :: Int-> Int -> Int -> Int -> 
connectionLine a b = fromVertices [a,b] # lw 0.5 # lc green
connectionTrail a b c d = strokeLoop (glueLine (paralellogram a b c d )) # fillRule EvenOdd # fc black 
paralellogram :: P2 -> P2 -> P2 -> P2 -> Trail' Line R2
paralellogram a b c d = lineFromVertices [a,b,d,c,a]

                          
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
