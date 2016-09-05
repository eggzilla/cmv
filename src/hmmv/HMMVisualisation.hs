{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Visualize Hidden Markov Models

module Main where

import qualified Bio.HMMParser as HM
import Bio.HMMDraw
import System.Console.CmdArgs
import Data.Either.Unwrap
import System.Directory

options :: Options
data Options = Options            
  { modelFile :: String,
    modelDetail :: String,
    modelLayout :: String
  } deriving (Show,Data,Typeable)


options = Options
  { modelFile = def &= name "m" &= help "Path to hidden Markov model file",
    modelDetail = "detailed" &= name "d" &= help "Set verbosity of drawn models: simple, detailed",
    modelLayout = "flat" &= name "l" &= help "Set layout of drawn models: flat, tree"
  } &= summary "HMMvisualisation devel version" &= help "Florian Eggenhofer - 2016" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  modelFileExists <- doesFileExist modelFile
  if modelFileExists
     then do
       model <- HM.readHMMER3 modelFile
       --print model
       if (isRight model) then printSVG svgsize (drawHMMER3 modelDetail (fromRight model)) else print (fromLeft model)
     else do
       putStrLn "Input model file not found"
