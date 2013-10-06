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
import Diagrams.Path
import Diagrams.Backend.SVG
import Data.Colour.SRGB
import Graphics.SVGFonts.ReadFont
  
-- | Draw one or more CM guide trees and concatenate them vertically
-- drawCMGuideForest :: (Renderable a b) => [[(String,String)]] -> QDiagram b0 R2 Any
drawCMGuideForest modelDetail cms comparisonshighlightparameter 
  | modelDetail == "simple" = alignTL (vcat' with { sep = 8 } (drawCMGuideTrees modelDetail  cms)) <> (mconcat (highlightComparisonTrails modelDetail comparisonshighlightparameter))
  | modelDetail == "detailed" = alignTL (vcat' with { sep = 40 } (drawCMGuideTrees modelDetail cms)) <> (mconcat (highlightComparisonTrails modelDetail comparisonshighlightparameter))
  | otherwise = alignTL (vcat' with { sep = 40 } (drawCMGuideTrees modelDetail cms)) <> (mconcat (highlightComparisonTrails modelDetail comparisonshighlightparameter))

-- | Highlight comparison by connecting the delimiting nodes of the aligned nodes of both models
-- takes the model identifier of both models and the starting and ending nodes of both models as arguments.
-- highlightComparisonLines ::
--highlightComparisonLines a b c d e f g h = highlightComparisonIntervalBoundry a b c d <> highlightComparisonIntervalBoundry e f g h

-- highlightComparisonIntervalBoundry ::
highlightComparisonIntervalBoundry model1index node1index model2index node2index = connectionLine (getNodeCoordinates "detailed" model1index node1index) (getNodeCoordinates "detailed" model2index node2index)


highlightComparisonTrails modelDetail trails  = map (highlightComparisonTrail modelDetail) trails

-- | Highlight comparison by connecting the all of the aligned nodes of both models
-- highlightComparisonTrail ::
highlightComparisonTrail modelDetail (a,b,c,d,e,f,g,h) = connectionTrail (getNodeCoordinates modelDetail a b) (getNodeCoordinates modelDetail c d) (getNodeCoordinates  modelDetail e f) (getNodeCoordinates modelDetail g h)

-- | Returns the center coordinates for an Covariance model guide tree node
getNodeCoordinates :: String -> Int -> Int -> P2
getNodeCoordinates modelDetail modelindex nodeindex 
   | modelDetail == "detailed" = p2 (fromIntegral x, fromIntegral y)
   | modelDetail == "simple" = p2 (fromIntegral a, fromIntegral b)
   | otherwise = p2 (fromIntegral x, fromIntegral y)
      where y = (getYCoordinateDetailed modelindex 0) * (-1)
            x = (5 + (10 * (nodeindex - 1 )))
            a = (1 + (2 * (nodeindex - 1 )))
            b = (getYCoordinateSimple modelindex 0) * (-1)


-- |  Computes the y coodinate for comparison highlighting, so that the
-- line or area starts at the lower edge of the cm representation and ends right above it
getYCoordinateDetailed :: Int -> Int -> Int 
getYCoordinateDetailed modelindex ycoordinate
    | modelindex == 0 = ycoordinate 
    | even modelindex = getYCoordinateDetailed (modelindex - 1) (ycoordinate + 40) 
    | odd modelindex = getYCoordinateDetailed (modelindex - 1) (ycoordinate + 10) 

getYCoordinateSimple :: Int -> Int -> Int 
getYCoordinateSimple modelindex ycoordinate
    | modelindex == 0 = ycoordinate 
    | even modelindex = getYCoordinateSimple (modelindex - 1) (ycoordinate + 8) 
    | odd modelindex = getYCoordinateSimple (modelindex - 1) (ycoordinate + 2) 

-- connectionLine :: Int-> Int -> Int -> Int -> 
connectionLine a b = fromVertices [a,b] # lw 0.5 # lc green

-- connectionTrail ::
connectionTrail a b c d = stroke (paralellogram a b c d ) # fc aqua # fillRule EvenOdd # lc black # lw 0.1
mkPath a b c d = position [a,b,c,d,a]
              
paralellogram :: P2 -> P2 -> P2 -> P2 -> Path R2
paralellogram a b c d = pathFromTrailAt (closeTrail (trailFromVertices [a,b,d,c,a])) a

-- | Draw the Guide Trees of multiple CMs, utilizes drawCMGuideNode
-- drawCMGuideTree  ->
drawCMGuideTrees detail cms  = map (drawCMGuideTree detail) cms

-- | Draw the guide Tree of a single CM, utilizes drawCMGuideNode
-- drawCMGuideTree  ->
drawCMGuideTree modelDetail nodes 
   | modelDetail == "simple" = hcat (drawCMGuideNodesSimple nodes)
   | modelDetail == "detailed" = hcat (drawCMGuideNodesVerbose nodes)

-- | Draw the guide Tree of a single CM, utilizes drawCMGuideNode
-- drawCMGuideNodes :: [(String,String)] ->  [Diagram b0 R2]           
drawCMGuideNodesVerbose nodes = map drawCMGuideNodeVerbose nodes
drawCMGuideNodesSimple nodes = map drawCMGuideNodeSimple nodes                                       
-- | Draws the guide tree nodes of a CM, verbose with label and index
-- drawCMGuideNode :: (String,String) ->  Diagram b0 R2
drawCMGuideNodeVerbose (number,label) =  text' label # translate (r2 (0,2)) <> text' number # translate (r2 (0,-2)) <> rect 10 10 # lw 0.5 # fc (labelToColor label)

-- | Draws the guide tree nodes of a CM, simplified
-- drawCMGuideNode :: (String,String) ->  Diagram b0 R2
drawCMGuideNodeSimple (_,label) =  rect 2 2 # lw 0.1 # fc (labelToColor label)

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
-- | Specifies the size of the diagram. Absolute adapts to overall size according to subdiagrams
--svgsize processedCMs = mkSizeSpec (svgwidth processedCMs) (svglength processedCMs)
svgsize = Absolute
diagramName = "./testdiagram.svg"

-- | Print drawn diagram as svg, already curried with diagram name, svgsize and the drawing have to specified
printSVG = renderSVG diagramName 
