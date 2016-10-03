-- | Drawing of covariance model (http://www.tbi.univie.ac.at/software/cmcompare/) guide trees and highlighting comparison results
-- Drawing is done with the diagrams package
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes #-}

module Bio.CMDraw
    (
     drawCMComparisons,
     drawSingleCMComparisons,
     drawCMs,
     drawSingleCMs,
     drawCM,
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
import qualified Data.Vector.Unboxed as VU
import qualified Data.PrimitiveArray.Index.PhantomInt as PI
import qualified Data.PrimitiveArray.Class as PA
import Biobase.SElab.Bitscore
import Biobase.Primary.Nuc.RNA
import qualified Data.PrimitiveArray.Index.Class as PAI
import Text.Printf
import Biobase.SElab.Types
import Data.Colour
import qualified Data.Colour.SRGB.Linear as R

-- | Draw one or more CM guide trees and concatenate them vertically
drawCMComparisons modelDetail cms alns comparisons
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (map (drawCM modelDetail) zippedInput)) <> (mconcat (highlightComparisonTrails modelDetail comparisonsHighlightParameter))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 40 } (map (drawCM modelDetail) zippedInput)) <> (mconcat (highlightComparisonTrails modelDetail comparisonsHighlightParameter))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (map (drawCM modelDetail) zippedInput))   
  | otherwise = alignTL (vcat' with { _sep = 40 } (map (drawCM modelDetail) zippedInput))
  where zippedInput = zip4 cms alns blankComparisonNodeLabels colorList
        blankComparisonNodeLabels = map getBlankComparisonNodeLabels cms
        colorList = replicate (length cms) white
	comparisonsHighlightParameter = getComparisonsHighlightParameters cms comparisons

-- | Draw one or more CM 
drawSingleCMComparisons modelDetail cms alns comparisons
  | modelDetail == "flat" = map (drawCM modelDetail) zippedInput -- <> (mconcat (highlightComparisonTrails modelDetail comparisonsHighlightParameter))
  | modelDetail == "simple" = map (drawCM modelDetail) zippedInput -- <> (mconcat (highlightComparisonTrails modelDetail comparisonsHighlightParameter))
  | modelDetail == "detailed" = map (drawCM modelDetail) zippedInput
  | otherwise = map (drawCM modelDetail) zippedInput -- <> (mconcat (highlightComparisonTrails modelDetail comparisonsHighlightParameter))
  where zippedInput = zip4 cms alns blankComparisonNodeLabels colorList
        blankComparisonNodeLabels = map getBlankComparisonNodeLabels cms
        colorList = replicate (length cms) white
	comparisonsHighlightParameter = getComparisonsHighlightParameters cms comparisons

-- | Draw one or more CM and concatenate them vertically
drawCMs modelDetail cms alns
  | modelDetail == "flat" = alignTL (vcat' with { _sep = 8 } (map (drawCM modelDetail) zippedInput))
  | modelDetail == "simple" = alignTL (vcat' with { _sep = 8 } (map (drawCM modelDetail) zippedInput))
  | modelDetail == "detailed" = alignTL (vcat' with { _sep = 40 } (map (drawCM modelDetail) zippedInput))
  | otherwise = alignTL (vcat' with { _sep = 40 } (map (drawCM modelDetail) zippedInput))
    where zippedInput = zip4 cms alns blankComparisonNodeLabels colorList
          blankComparisonNodeLabels = map getBlankComparisonNodeLabels cms
          colorList = replicate (length cms) white

-- | Draw one or more CM 
drawSingleCMs modelDetail cms alns
  | modelDetail == "flat" = map (drawCM modelDetail) zippedInput
  | modelDetail == "simple" = map (drawCM modelDetail) zippedInput
  | modelDetail == "detailed" = map (drawCM modelDetail) zippedInput
  | otherwise = map (drawCM modelDetail) zippedInput
    where zippedInput = zip4 cms alns blankComparisonNodeLabels colorList
          blankComparisonNodeLabels = map getBlankComparisonNodeLabels cms
          colorList = replicate (length cms) white

-- | Draw the guide Tree of a single CM
--drawCM :: forall n b. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => [Char] -> [(String, [Char])] -> QDiagram b V2 n Any
drawCM modelDetail (cm,aln,blankComparisonNodeLabels,modelColor)
   | modelDetail == "flat" = vcat (map drawCMNodeFlat nodes1)
   | modelDetail == "simple" = vcat (map drawCMNodeSimple nodes1)
   | modelDetail == "detailed" = detailedModelAlignment
   | otherwise = detailedModelAlignment
   where nodes1 = (processCMGuideTree cm)
         nodes = CM._nodes cm
         nodeNumber = V.length nodes
         allStates = CM._states cm
         boxlength = fromIntegral (length alphabetSymbols) + 2
         alphabetSymbols = ['A','U','C','G']
         emissiontype = "bar"
	 --nullModel = CM._nullModel cm
	 --nullModelBitscores = VU.toList (VU.map (getBitscore) nullModel)
	 --dummyLetters = replicate (length nullModelBitscores) "I"
	 --dummyNullModelBitscores = zip dummyLetters nullModelBitscores
	 --nullModelBox = vcat (map (emissionEntry "score") dummyNullModelBitscores)
	 modelName = CM._name cm
	 detailedModelAlignment = alignTL (vcat' with { _sep = 5 }  [modelHeader,detailedModel]) -- ,alignmentDiagram])
	 modelHeader = makeModelHeader (T.unpack modelName) modelColor
	 nodeIndices = V.iterateN nodeNumber (1+) 0
	 detailedModel = vcat (V.toList (V.map (drawCMNodeDetailed alphabetSymbols emissiontype boxlength (0 :: Int) nodeNumber nodeNumber V.empty allStates nodes) nodeIndices))

makeModelHeader mName modelColor = strutX 2 ||| hcat (map setTitelLetter mName) ||| strutX 1 ||| rect 4 4 # lw 0.1 # fc modelColor
setTitelLetter echar = alignedText 0.5 0.5 [echar] # fontSize 4.0 <> rect 4.0 4.0 # lw 0

-- | Draws the guide tree nodes of a CM, simplified
--drawCMNodeSimple :: forall t b. (Data.Typeable.Internal.Typeable (N b), TrailLike b, HasStyle b, V b ~ V2) => (t, [Char]) -> b
drawCMNodeFlat (_,label) =  rect 2 2 # lw 0.1 # fc (labelToColor label)

-- | Draws the guide tree nodes of a CM, verbose with label and index
--drawCMGuideNodeVerbose :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => (String, String) -> QDiagram b V2 n Any
drawCMNodeSimple (number,label) = text' label # translate (r2 (0,2)) <> text' number # translate (r2 (0,negate 2)) <> rect 10 10 # lw 0.5 # fc (labelToColor label)

text' t = alignedText 0.5 0.5 t # fontSize 2 <> rect textLength 2 # lw 0.0
  where textLength = fromIntegral (length t) * 2

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

--drawCMNodeDetailed :: [Char] -> String -> Double -> Int -> Int -> Int -> V.Vector (Int, V.Vector (Colour Double)) -> CM.States -> CM.Node -> QDiagram b V2 n Any
drawCMNodeDetailed alphabetSymbols emissiontype boxlength rowStart rowEnd lastIndex comparisonNodeLabels states nodes nodeIndex 
  -- | idNumber == 0 = beginBox
  -- | idNumber == (lastIndex - 1) = nodeBox ||| endBox
  -- | idNumber == rowStart = rowStartBox idNumber boxlength ||| nodeBox
  -- | idNumber == rowEnd - 1 = nodeBox ||| rowEndBox idNumber boxlength                    
  | otherwise = nodeBox 
  where node = nodes V.! nodeIndex
        idNumber = CM._nid node
        nid = show idNumber
	nodeBox = drawCMNodeBox alphabetSymbols emissiontype boxlength states node

drawCMNodeBox alphabetSymbols emissiontype boxlength currentStates node
  | ntype == CM.NodeType 0 = bifNode # translate (r2 (negate 23,12.5)) <> nodeBox
  | ntype == CM.NodeType 1 = matPNode # translate (r2 (negate 23,12.5)) <> nodeBox
  | ntype == CM.NodeType 2 = matLNode # translate (r2 (negate 23,12.5)) <> nodeBox
  | ntype == CM.NodeType 3 = matRNode # translate (r2 (negate 23,12.5)) <> nodeBox
  | ntype == CM.NodeType 4 = begLNode # translate (r2 (negate 23,12.5)) <> nodeBox
  | ntype == CM.NodeType 5 = begRNode # translate (r2 (negate 23,12.5)) <> nodeBox
  | ntype == CM.NodeType 6 = rootNode # translate (r2 (negate 23,12.5)) <> nodeBox
  | ntype == CM.NodeType 7 = endNode # translate (r2 (negate 23,12.5)) <> nodeBox
  | otherwise = endNode <> nodeBox
    where ntype = CM._ntype node
          idNumber = PI.getPInt (CM._nid node)
          nid = show idNumber
          stateIndices = VU.toList (CM._nstates node)               
          splitStatesBox = hcat (map (drawCMSplitStateBox nid alphabetSymbols emissiontype boxlength currentStates) stateIndices)
	  insertStatesBox = hcat (map (drawCMInsertStateBox nid alphabetSymbols emissiontype boxlength currentStates) stateIndices)
          --insertStatesBox = if (length insertStates) == 2 then hcat [(insertStates !! 0),(strutX (0)),(insertStates !! 1)] else hcat [(insertStates !! 0),(strutX (0))] 
          -- bif b
          bifNode = (text' "BIF" # rotate (1/4 @@ turn) ||| strutX 2.0 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- matP mp ml mr d il ir
          matPNode = (text' "MATP" # rotate (1/4 @@ turn) ||| strutX 2.0 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- matL ml d il
          matLNode = (text' "MATL" # rotate (1/4 @@ turn) ||| strutX 2.0 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- matR mr d ir
          matRNode = (text' "MATR" # rotate (1/4 @@ turn) ||| strutX 2.0 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- begL s
          begLNode = (text' "BEGL" # rotate (1/4 @@ turn) ||| strutX 2.0 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- begR s il
          begRNode = (text' "BEGR" # rotate (1/4 @@ turn) ||| strutX 2.0 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- root s il ir
          rootNode = (text' "ROOT" # rotate (1/4 @@ turn) ||| strutX 2.0 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
          -- end e
          endNode = (text' "END" # rotate (1/4 @@ turn) ||| strutX 2.0 ||| splitStatesBox) === strutY 5.0 === insertStatesBox
                    
nodeBox = rect 60 60 # lw 0.1

--drawCMStateBox :: [Char]  -> String -> Double -> CM.States -> PI.PInt () CM.StateIndex -> QDiagram NullBackend V2 n Any
drawCMSplitStateBox nid alphabetSymbols emissiontype boxlength currentStates sIndex
  | stype == CM.StateType 0 = dState # translate (r2 (negate 3,8)) <> statebox 8.0 20.0 # named (nid ++"d")
  | stype == CM.StateType 1 = mpState # translate (r2 (negate 7,8)) <> statebox 16.0 20.0 # named (nid ++"mp")
  | stype == CM.StateType 2 = mlState # translate (r2 (negate 3,8)) <> statebox 8.0 20.0 # named (nid ++"ml")
  | stype == CM.StateType 3 = mrState # translate (r2 (negate 3,8)) <> statebox 8.0 20.0 # named (nid ++"mr")
  | stype == CM.StateType 6 = sState # translate (r2 (negate 3,8)) <> statebox 8.0 20.0 # named (nid ++"s")
  | stype == CM.StateType 7 = eState # translate (r2 (negate 3,8)) <> statebox 8.0 20.0 # named (nid ++"e")
  | stype == CM.StateType 8 = bState # translate (r2 (negate 3,8)) <> statebox 8.0 20.0 # named (nid ++"b")
  | stype == CM.StateType 9 = elState # translate (r2 (negate 3,8)) <> statebox 8.0 20.0 # named (nid ++"el")                                               
  | otherwise = mempty
    where stype = (CM._sStateType currentStates) PA.! sIndex
          singleEmissionBitscores = V.map ((score2Prob 1.0) . ((CM._sSingleEmissions currentStates) PA.!)) (makeSingleEmissionIndices sIndex)
          singleEmissionEntries = setEmissions emissiontype singleEmissionBitscores
          singleSymbolsAndEmissions = zip ["A","U","G","C"] (V.toList singleEmissionEntries)
	  pairEmissionBitscores = V.map ((score2Prob 1.0). ((CM._sPairEmissions currentStates) PA.!)) (makePairEmissionIndices sIndex)
          pairEmissionEntries = setEmissions emissiontype pairEmissionBitscores
	  pairSymbolsAndEmissions = zip ["AA","AU","AG","AC","UU","UA","UG","UC","GG","GA","GU","GC","CC","CA","CU","CG"] (V.toList pairEmissionEntries)
	  pairSymbolsAndEmissions1 = take 8 pairSymbolsAndEmissions
	  pairSymbolsAndEmissions2 = drop 8 pairSymbolsAndEmissions
          dState = text' "D" # translate (r2 (3,0.5)) === strutX 1.5 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          mpState = text' "MP" # translate (r2 (7,0.5)) === strutX 1.5 === (vcat (map (emissionEntry emissiontype) pairSymbolsAndEmissions1) ||| strutX 0.5 ||| vcat (map (emissionEntry emissiontype) pairSymbolsAndEmissions2))
          mlState = text' "ML" # translate (r2 (3,0.5)) === strutX 1.5 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          mrState = text' "MR" # translate (r2 (3,0.5)) === strutX 1.5 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          sState = text' "S" # translate (r2 (3,0.5)) === strutX 1.5 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions)
          eState = text' "E" # translate (r2 (3,0.5)) === strutX 1.5 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          bState = text' "B" # translate (r2 (3,0.5)) === strutX 1.5 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          elState =  text' "EL" # translate (r2 (3,0.5)) === strutX 1.5 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          
drawCMInsertStateBox nid alphabetSymbols emissiontype boxlength currentStates sIndex
  | stype == CM.StateType 4 = (ilState # translate (r2 (negate 3,8)) <> statebox 8.0 20.0 # named (nid ++"il")) ||| strutX 40
  | stype == CM.StateType 5 = (irState # translate (r2 (negate 3,8)) <> statebox 8.0 20.0 # named (nid ++"ir")) 
  | otherwise = mempty
    where stype = (CM._sStateType currentStates) PA.! sIndex          
          singleEmissionBitscores = V.map ((score2Prob 1.0) . ((CM._sSingleEmissions currentStates) PA.!)) (makeSingleEmissionIndices sIndex)
          singleEmissionEntries = setEmissions emissiontype singleEmissionBitscores
          singleSymbolsAndEmissions = zip ["A","U","G","C"] (V.toList singleEmissionEntries)
          ilState = text' "IL" # translate (r2 (3,0.5)) === strutX 1.5 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 
          irState = text' "IR" # translate (r2 (3,0.5)) === strutX 1.5 === vcat (map (emissionEntry emissiontype) singleSymbolsAndEmissions) 

setEmissions :: String -> V.Vector Double -> V.Vector Double
setEmissions emissiontype emissions
  | emissiontype == "score" = scoreentries
  | emissiontype == "probability" = propentries
  | emissiontype == "bar" = barentries
  | otherwise = barentries
    where scoreentries = emissions      
          propentries = V.map (exp . negate) emissions
          barentries = V.map (exp . negate) emissions

wrap x = [x]
         
--emissionEntry :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => String -> (String,Double) -> QDiagram b V2 n Any
emissionEntry emissiontype (symbol,emission) 
  | emissiontype == "probability" = textentry
  | emissiontype == "score" = textentry
  | emissiontype == "bar" = barentry
  | otherwise = barentry
    where textentry = alignedText 0 0.1 (symbol ++ " " ++ printf "%.3f" emission) # translate (r2 (negate 0.5,0)) <> (rect 3 2 # lw 0 ) 
          --barentry =  stroke (textSVG symbol 2) ||| bar emission
          barentry = (alignedText 0 0.01 symbol  # translate (r2 (negate 0.5,negate 0.3)) <> (rect 3 2 # lw 0)) ||| bar emission

--bar :: forall b n. (Read n, RealFloat n, Data.Typeable.Internal.Typeable n, Renderable (Path V2 n) b) => Double -> QDiagram b V2 n Any
bar emission = (rect (4 * emission) 1 # lw 0 # fc black # translate (r2 (negate (2 - (4 * emission/2)),0)) <> rect 4 1 # lw 0.03 )
                    
makeSingleEmissionIndices index = V.fromList [(PAI.Z  PAI.:. index PAI.:. A),(PAI.Z  PAI.:. index PAI.:. U),(PAI.Z  PAI.:. index PAI.:. G),(PAI.Z  PAI.:. index PAI.:. C)]
-- AA AU AG AC UU UA UG UC GG GA GU GC CC CA CU CG
makePairEmissionIndices index = V.fromList [(PAI.Z  PAI.:. index PAI.:. A PAI.:. A),(PAI.Z  PAI.:. index PAI.:. A PAI.:. U),(PAI.Z  PAI.:. index PAI.:. A PAI.:. G),(PAI.Z  PAI.:. index PAI.:. A PAI.:. C),(PAI.Z  PAI.:. index PAI.:. U PAI.:. U),(PAI.Z  PAI.:. index PAI.:. U PAI.:. A),(PAI.Z  PAI.:. index PAI.:. U PAI.:. G),(PAI.Z  PAI.:. index PAI.:. U PAI.:. C),(PAI.Z  PAI.:. index PAI.:. G PAI.:. G),(PAI.Z  PAI.:. index PAI.:. G PAI.:. A),(PAI.Z  PAI.:. index PAI.:. G PAI.:. U),(PAI.Z  PAI.:. index PAI.:. G PAI.:. C),(PAI.Z  PAI.:. index PAI.:. C PAI.:. C),(PAI.Z  PAI.:. index PAI.:. C PAI.:. A),(PAI.Z  PAI.:. index PAI.:. C PAI.:. U),(PAI.Z  PAI.:. index PAI.:. C PAI.:.G)]

statebox x y = rect x y  # lw 0.1
       
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

getBlankComparisonNodeLabels :: CM.CM -> V.Vector (Int, V.Vector (Colour Double))
getBlankComparisonNodeLabels model = comparisonNodeLabels
   where comparisonNodeLabels = V.generate (nodeNumber +1 ) makeBlankComparisonNodeLabel
         nodeNumber = CM._nodesInModel model

makeBlankComparisonNodeLabel :: Int ->  (Int,(V.Vector (Colour Double)))
makeBlankComparisonNodeLabel nodeNumber = (nodeNumber,V.singleton white)

makeColorVector modelNumber = V.map (\(a,b,c) -> R.rgb a b c) modelRGBTupel
   where indexVector = V.iterateN (modelNumber) (1+) 0
         stepSize = (765 :: Double) / (fromIntegral modelNumber)
	 modelRGBTupel = V.map (makeRGBTupel stepSize) indexVector

--makeRGBTupel :: Double -> Int -> (Double,Double,Double)
makeRGBTupel stepSize modelNumber = (a,b,c)
  where  totalSize = (fromIntegral modelNumber) * stepSize 
         a = (rgbBoundries (totalSize  - 255))/255
	 b = (rgbBoundries (totalSize - a - 255))/255
	 c = (rgbBoundries (totalSize - a - b))/255

--rgbBoundries :: Double -> Double	     
rgbBoundries rgbValue
  | rgbValue>210 = 210
  | rgbValue<50 = 50
  | otherwise = rgbValue   