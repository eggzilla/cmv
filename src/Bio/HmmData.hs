-- | Datastructures for nHMMER hidden Markov models, representing RNA families
--   For more information on nHMMER consult <http://eddylab.org/infernal/>

module Bio.HmmData where

import qualified Data.Vectors as V

-- | Data structure for nHMMER
data Nhmmer = Nhmmer
  { 
    version :: String,
    name :: String,
    acc :: String,
    leng :: Int,
    maxl :: Int,
    alpha :: String,
    rf :: Bool,
    mm :: Bool,
    cons :: Bool,
    cs :: Bool,
    map :: Bool,
    date :: String,
    com :: String,
    nseq :: Int,
    effn :: Double,
    cksum :: Int,
    localmsv :: (Double,Double),
    localviterbi :: (Double,Double),
    localforward :: (Double,Double),
    states :: V.Vector Nhmmerstate
  }
  deriving (Show, Eq)

data Nhmmerstate = Nhmmerstate
  { 
    matchEmissions :: [Double],
    insertEmissions :: [Double],
    transitions :: [Double]
  }
  deriving (Show, Eq)
