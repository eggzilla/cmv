-- | Font test script
-- runghc -package-db --ghc-arg=../cabal-sandbox/x86_64-linux-ghc-7.10.3-packages.conf.d/ literalStringEscaper.hs
-- runghc -package-db --ghc-arg=../cabal-sandbox/x86_64-linux-ghc-8.0.1-packages.conf.d/ literalStringEscaper.hs

module Main where

import Prelude
import System.Environment (getArgs)
import System.Process 
import System.IO
import System.Environment
import Data.List
import System.Directory
import System.Process

main = do
     args <- getArgs
     let input_file = (head args)
     inputString <- readFile input_file
     print inputString 