-- | Drawing of covariance model (http://www.tbi.univie.ac.at/software/cmcompare/) guide trees and highlighting comparison results
-- Drawing is done with the diagrams package
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes #-}

module Bio.CMDraw
    (
     drawCMGuideForestComparison, 
     drawCMGuideForest,
     drawCMGuideTrees,
     drawCMGuideTree,
     text',
     svgsize,
     diagramName,
     printCM,
     processCMs,
     processCMGuideTree,
     getNodeInfo,
     sortModelsByComparisonResults,
     findModelError,
     findModel,
     findModelIndex,
     findMissing,
     getCMName,
     checkCMCResultsParsed,
     checkCMCResultParsed,
     checkSortedModels,
     getComparisonsHighlightParameters,
     getComparisonHighlightParameters,
     highlightComparisonIntervalBoundry,
     connectionLine,
     mkPath
    ) where
  
import Diagrams.Prelude
import Data.Typeable.Internal
import Bio.CMCompareResult
import qualified Biobase.SElab.CM as CM
import Data.List
import Text.Parsec.Error
import qualified Data.Text as T
import qualified Data.Vector as V
import Bio.StockholmDraw
import qualified Diagrams.Backend.Cairo as C
    
-- | Draw one or more CM guide trees and concatenate them vertically
--drawCMGuideForestComparison :: forall b. Renderable (Path V2 Double) b => [Char] -> [[(String, [Char])]] -> [(Int, Int, Int, Int, Int, Int, Int, Int)] -> QDiagram b V2 Double Any
drawCMGuideForestComparison modelDetail cms comparisonshighlightparameter 
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (drawCMGuideTrees modelDetail  cms)) <> (mconcat (highlightComparisonTrails modelDetail comparisonshighlightparameter))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 40 } (drawCMGuideTrees modelDetail cms)) <> (mconcat (highlightComparisonTrails modelDetail comparisonshighlightparameter))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (drawCMGuideTrees modelDetail cms)) <> (mconcat (highlightComparisonTrails modelDetail comparisonshighlightparameter))                            
  | otherwise = alignTL (vcat' with { _sep = 40 } (drawCMGuideTrees modelDetail cms)) <> (mconcat (highlightComparisonTrails modelDetail comparisonshighlightparameter))

-- | Draw one or more CM guide trees and concatenate them vertically
--drawCMGuideForest :: forall b. Renderable (Path V2 Double) b => [Char] -> [[(String, [Char])]] -> QDiagram b V2 Double Any
drawCMGuideForest modelDetail cms
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (drawCMGuideTrees modelDetail cms))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 8 } (drawCMGuideTrees modelDetail cms))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (drawCMGuideTrees modelDetail cms))
  | otherwise = alignTL (vcat' with { _sep = 40 } (drawCMGuideTrees modelDetail cms))

-- | Highlight comparison by connecting the delimiting nodes of the aligned nodes of both models
-- takes the model identifier of both models and the starting and ending nodes of both models as arguments.
--highlightComparisonLines a b c d e f g h = highlightComparisonIntervalBoundry a b c d <> highlightComparisonIntervalBoundry e f g h


highlightComparisonIntervalBoundry :: forall b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2, N b ~ Double) => Int -> Int -> Int -> Int -> b
highlightComparisonIntervalBoundry model1index node1index model2index node2index = connectionLine (getNodeCoordinates "detailed" model1index node1index) (getNodeCoordinates "detailed" model2index node2index)

highlightComparisonTrails :: forall b. Renderable (Path V2 Double) b => String -> [(Int, Int, Int, Int, Int, Int, Int, Int)] -> [QDiagram b V2 Double Any]
highlightComparisonTrails modelDetail trails  = map (highlightComparisonTrail modelDetail) trails

-- | Highlight comparison by connecting the all of the aligned nodes of both models
highlightComparisonTrail :: forall b. Renderable (Path V2 Double) b => String -> (Int, Int, Int, Int, Int, Int, Int, Int) -> QDiagram b V2 Double Any
highlightComparisonTrail modelDetail (a,b,c,d,e,f,g,h) = connectionTrail (getNodeCoordinates modelDetail a b) (getNodeCoordinates modelDetail c d) (getNodeCoordinates  modelDetail e f) (getNodeCoordinates modelDetail g h)

-- | Returns the center coordinates for an Covariance model guide tree node
getNodeCoordinates :: String -> Int -> Int -> P2 Double
getNodeCoordinates modelDetail modelindex nodeindex 
   | modelDetail == "simple" = p2 (fromIntegral x, fromIntegral y)
   | modelDetail == "flat" = p2 (fromIntegral a, fromIntegral b)
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
    | otherwise = getYCoordinateDetailed (modelindex - 1) (ycoordinate + 10)
    

getYCoordinateSimple :: Int -> Int -> Int 
getYCoordinateSimple modelindex ycoordinate
    | modelindex == 0 = ycoordinate 
    | even modelindex = getYCoordinateSimple (modelindex - 1) (ycoordinate + 8) 
    | otherwise = getYCoordinateSimple (modelindex - 1) (ycoordinate + 2) 

connectionLine :: forall b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => Point (V b) (N b) -> Point (V b) (N b) -> b
connectionLine a b = fromVertices [a,b] # lw 0.5 # lc green

connectionTrail :: forall b n. (RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => Point V2 n -> Point V2 n -> Point V2 n -> Point V2 n -> QDiagram b V2 n Any
connectionTrail a b c d = stroke (paralellogram a b c d ) # fc aqua # fillRule EvenOdd # lc black # lw 0.1

mkPath :: forall a. (Num (N a), Monoid a, Semigroup a, Additive (V a), HasOrigin a) => (Point (V a) (N a), a) -> (Point (V a) (N a), a) -> (Point (V a) (N a), a) -> (Point (V a) (N a), a) -> a
mkPath a b c d = position [a,b,c,d,a]
              
paralellogram :: forall (v :: * -> *) n. (Floating n, Ord n, Metric v) => Point v n -> Point v n -> Point v n -> Point v n -> Path v n
paralellogram a b c d = pathFromTrailAt (closeTrail (trailFromVertices [a,b,d,c,a])) a

-- | Draw the Guide Trees of multiple CMs, utilizes drawCMGuideNode
--drawCMGuideTrees :: forall n b. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => [Char] -> [[(String, [Char])]] -> [QDiagram b V2 n Any]
drawCMGuideTrees detail cms  = map (drawCMGuideTree detail) cms

-- | Draw the guide Tree of a single CM, utilizes drawCMGuideNode
--drawCMGuideTree :: forall n b. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => [Char] -> [(String, [Char])] -> QDiagram b V2 n Any
drawCMGuideTree modelDetail cm
   | modelDetail == "flat" = hcat (drawCMNodesSimple (processCMGuideTree cm))
   | modelDetail == "simple" = hcat (drawCMNodesVerbose (processCMGuideTree cm))
   | modelDetail == "detailed" = hcat (drawCMNodesDetailed cm)                            
   | otherwise = hcat (drawCMGuideNodesSimple (processCMGuideTree cm))

-- | Draw the guide Tree of a single CM, utilizes drawCMGuideNode
--drawCMGuideNodesVerbose :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => [(String, String)] -> [QDiagram b V2 n Any]
drawCMNodesVerbose nodes = map drawCMGuideNodeVerbose nodes

drawCMNodesSimple :: forall b t. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => [(t, [Char])] -> [b]
drawCMNodesSimple nodes = map drawCMGuideNodeSimple nodes

-- | Draws the guide tree nodes of a CM, verbose with label and index
--drawCMGuideNodeVerbose :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => (String, String) -> QDiagram b V2 n Any
drawCMNodeVerbose (number,label) = text' label # translate (r2 (0,2)) <> text' number # translate (r2 (0,negate 2)) <> rect 10 10 # lw 0.5 # fc (labelToColor label)

-- | Draws the guide tree nodes of a CM, simplified
drawCMNodeSimple :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => (t, [Char]) -> b
drawCMNodeSimple (_,label) =  rect 2 2 # lw 0.1 # fc (labelToColor label)

text' t = alignedText 0.5 0.5 t # fontSize 4 <> rect 4 4 # lw 0.0

-- | Transform covariance model node labels to colors
labelToColor :: forall b. (Floating b, Ord b) => [Char] -> Colour b
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

drawCMNodesDetailed cm = map (drawCMNodeDetailed alphabetSymbols emissiontype boxlength (0 :: Int) nodeNumber  nodeNumber V.empty states) nodes
  where nodes = _nodes cm
        nodeNumber = V.length nodes
        states = _states cm

drawCMNodeDetailed alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex comparisonNodeLabels states node 
  -- | idNumber == 0 = beginBox
  -- | idNumber == (lastIndex - 1) = nodeBox ||| endBox
  -- | idNumber == rowStart = rowStartBox idNumber boxlength ||| nodeBox
  -- | idNumber == rowEnd - 1 = nodeBox ||| rowEndBox idNumber boxlength                    
  | otherwise = nodeBox alphabetSymbols emissiontype boxlength node
  where idNumber = _nid node
        nid  = show idNumber
	nodeBox = drawCMNodeBox alphabetSymbols emissiontype boxlength states node

drawCMNodeBox alphabetSymbols emissiontype boxlength states node
  | type == 0 = matchBox
  | type == 1 = matchBox
  | type == 2 = matchBox
  | type == 3 = matchBox
  | type == 4 = matchBox
  | type == 5 = matchBox
  | type == 6 = matchBox
  | type == 7 = matchBox
  | otherwise = matchBox
    where matchBox = rect 1 1 
          type = _ntype node 

 --    "BIF"  -> Bif
 --     "MATP" -> MatP
 --     "MATL" -> MatL
 --     "MATR" -> MatR
 --     "BEGL" -> BegL
 --     "BEGR" -> BegR
 --     "ROOT" -> Root
 --     "END"  -> End

--scaling
-- | Specifies the size of the diagram. Absolute adapts to overall size according to subdiagrams
svgsize :: SizeSpec V2 Double
svgsize = mkSizeSpec2D Nothing Nothing
           
-- | Check for available cairo output formats
diagramName :: String -> String -> Either String String
diagramName filename fileformat
  | fileformat == "pdf" = Right (filename ++ "." ++ fileformat )
  | fileformat == "svg" = Right (filename ++ "." ++ fileformat )
  | fileformat == "png" = Right (filename ++ "." ++ fileformat )
  | fileformat == "ps" = Right (filename ++ "." ++ fileformat )
  | otherwise = Left "Unsupported output format requested (use svg, pdf, ps, png)"

printCM outputName = C.renderCairo outputName
                
processCMs :: [CM.CM] -> [[(String,String)]]
processCMs cms = map processCMGuideTree cms

processCMGuideTree :: CM.CM -> [(String,String)]
--processCMGuideTree cm = map getNodeInfo (Map.assocs (CM._nodes cm))
processCMGuideTree cm = map getNodeInfo (V.toList (CM._nodes cm))

--getNodeInfo :: (CM.Node, (CM.NodeType, [CM.State])) -> (String,String)
--getNodeInfo (nodeid, (nodetype, _ )) = (show (CM._nId nodeid) , (show nodetype))

getNodeInfo :: CM.Node -> (String,String)
getNodeInfo _node = (show (CM._nid _node) , show (CM._ntype _node))

sortModelsByComparisonResults :: [String] -> [CM.CM] -> [Either String CM.CM]
sortModelsByComparisonResults cmComparisonNames models = map (\x -> findModelError x (findModel x models)) cmComparisonNames
-- todo: also add models at end of the sorted list that are not in comparisons
-- ++ (map (\x -> findModelError x (findMissing x models)) cmComparisonNames)

findModelError :: String -> Maybe CM.CM -> Either String CM.CM
findModelError _name (Just model) = Right model
findModelError _name Nothing = Left ("Model " ++ _name ++ "that is present in comparison file is not present in model file")

findModel :: String -> [CM.CM] -> Maybe CM.CM
findModel check models = find (\x -> getCMName x == check) models

findModelIndex :: String -> [CM.CM] -> Maybe Int
findModelIndex check models = findIndex (\x -> getCMName x == check) models

findMissing :: String -> [CM.CM] -> Maybe CM.CM
findMissing check models = find (\x -> getCMName x /= check) models

getCMName :: CM.CM -> String
getCMName x = filter (\c -> c /= ' ')  (T.unpack (CM._name x))

checkCMCResultsParsed :: [ParseError] -> IO ()
checkCMCResultsParsed x 
  | null x = print "Parsing comparisons - done\n"
  | otherwise = error ("Following errors occured:" ++ (concat (map checkCMCResultParsed x)))

checkCMCResultParsed :: ParseError -> String
checkCMCResultParsed x = (concat (map messageString (errorMessages x)))
--  | (errorIsUnknown x) = print "Parsing comparisons - done\n" 
--  | x == [] = print "Parsing comparisons - done\n"
--  | otherwise = (concat (map messageString (errorMessages x)))
--  | otherwise = error ("Following errors occured :" ++ (concat (map (\y -> (concat (map messageString (errorMessages y)))) x)))

checkSortedModels :: forall a. (Eq a, Show a) => [[a]] -> IO ()
checkSortedModels x
  | x == [] = print "Sorting input models to comparison list - done\n" 
  | otherwise = error ("Following errors occured :" ++ show (concat x))

getComparisonsHighlightParameters :: [CM.CM] -> [CmcompareResult] -> [(Int,Int,Int,Int,Int,Int,Int,Int)]
getComparisonsHighlightParameters sortedmodels comp = map (getComparisonHighlightParameters sortedmodels) comp

getComparisonHighlightParameters :: [CM.CM] -> CmcompareResult -> (Int,Int,Int,Int,Int,Int,Int,Int)
getComparisonHighlightParameters sortedmodels comp = (a,b,c,d,a,f,c,e)
  where --a = (fromJust (findModelIndex (model1Name comp) sortedmodels) + 1)
        a = 1
        b = head (model1matchednodes comp)
        --c = (fromJust (findModelIndex (model2Name comp) sortedmodels) + 1)
        c = 2
        d = head (model2matchednodes comp)
        e = last (model2matchednodes comp)
        f = last (model1matchednodes comp)
