{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Visualize Hidden Markov Models

module Main where

import Bio.HMMParser
import System.Console.CmdArgs
import Data.Either
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
       model <- readHMMER3 modelFile
       print model
       --printSVG svgsize (drawHMMER3 modelDetail (processHMMER3 (model)))
     else do
       putStrLn "Input model file not found"
