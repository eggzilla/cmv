-- | Datastructures for nHMMER hidden Markov models, representing RNA families
--   For more information on nHMMER consult <http://eddylab.org/infernal/>

module Bio.HmmData where

import qualified Data.Vectors as V

-- | Data structure for nHMMER
data Nhmmer3 = Nhmmer3
  {
    -- file format version; mandatory
    version :: String,
    -- model name; mandatory
    name :: String,
    -- accession number; optional
    acc :: Maybe String,
    -- description; optional
    desc :: Maybe String,
    -- model length; mandatory
    leng :: Int,
    -- maximum instance length; optional
    maxl :: Maybe Int,
    -- symbol alphabet type either amino, RNA or DNA; mandatory
    alpha :: String,
    -- reference annotation; optional
    rf :: Bool,
    -- model masked; optional
    mm :: Bool,
    -- consensus residue annotation flag; Mandatory
    cons :: Bool,
    -- consensus structure; optional
    cs :: Maybe Bool,
    -- map annotation; optional
    map :: Bool,
    -- date; optional
    date :: String,
    -- command line log; optional
    com :: Maybe String,
    -- sequence number; optional
    nseq :: Int,
    -- effective sequence number; optional
    effn :: Double,
    -- training alignment checksum; optional
    cksum :: Int,
    -- Pfam gathering treshold; optional
    ga :: Maybe (Double, Double),
    -- Pfam trusted cutoff; optional
    tc :: Maybe (Double, Double),
    -- Pfam noise cutoff; optional
    nc :: Maybe (Double, Double),
    --stats
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
