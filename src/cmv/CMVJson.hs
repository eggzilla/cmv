{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | TaxIds2Tree
-- dist/build/CMVJson/CMVJson -i input.cm
module Main where

import Prelude 
import System.Console.CmdArgs
import qualified Biobase.SElab.CM as CM
import Biobase.SElab.CM.Import (cmFromFile)
import Data.Either.Unwrap
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char
import qualified Data.Aeson.Encode as E

--------------------------------------------------------

data Options = Options            
  { inputModelFile :: String,
    outputDirectoryPath :: String
  } deriving (Show,Data,Typeable)

options :: Options
options = Options
  { inputModelFile = def &= name "m" &= help "Path to input",
    outputDirectoryPath = def &= name "o" &= help "Path to output directory"
  } &= summary "CMVJson -  CM is converted into a graphical tree representation via .json (via d3js)" &= help "Florian Eggenhofer - 2017" &= verbosity   

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  cms <- cmFromFile inputModelFile
  if not (null cms)
    then do     
      let jsonCM = E.encode (head cms)
      print jsonCM
    else print "No models"

--generateJsonOutput :: String -> Gr SimpleTaxon Double -> IO ()
--generateJsonOutput outputDirectoryPath inputGraph = do
--  let jsonOutput = E.encode (grev inputGraph)
--  L.writeFile (outputDirectoryPath ++ "model.json") jsonOutput

