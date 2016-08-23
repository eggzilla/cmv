-- | Datastructures for nHMMER hidden Markov models, representing RNA families
--   For more information on nHMMER consult <http://eddylab.org/infernal/>

module Bio.HmmData where

import qualified Data.Vectors as V

-- | Data structure for HMMER
data HMMER3 = HMMER3
  {
    -- File format version; mandatory
    version :: String,
    -- Model name; mandatory
    name :: String,
    -- Accession number; optional
    acc :: Maybe String,
    -- Description; optional
    desc :: Maybe String,
    -- Model length; mandatory
    leng :: Int,
    -- Maximum instance length; optional
    maxl :: Maybe Int,
    -- Symbol alphabet type either amino, RNA or DNA; mandatory
    alpha :: String,
    -- Reference annotation; optional
    rf :: Bool,
    -- Model masked; optional
    mm :: Bool,
    -- Consensus residue annotation flag; Mandatory
    cons :: Bool,
    -- Consensus structure; optional
    cs :: Maybe Bool,
    -- Map annotation; optional
    map :: Bool,
    -- Date; optional
    date :: String,
    -- Command line log; optional
    com :: Maybe String,
    -- Sequence number; optional
    nseq :: Int,
    -- Effective sequence number; optional
    effn :: Double,
    -- Training alignment checksum; optional
    cksum :: Int,
    -- Pfam gathering treshold; optional
    ga :: Maybe (Double, Double),
    -- Pfam trusted cutoff; optional
    tc :: Maybe (Double, Double),
    -- Pfam noise cutoff; optional
    nc :: Maybe (Double, Double),
    -- stats for E-value calculation describing location and slope of distribution; optional
    -- MSV - Gumpel distribution; optional
    localmsv :: (Double,Double),
    -- Viterbi - Gumpel distribution; optional
    localviterbi :: (Double,Double),
    -- Forward - exponential tail fitting; optional
    localforward :: (Double,Double),
    -- Symbol characters for this alphabet; mandatory
    hmm :: V.Vector Char
    -- Models overall (composite) match state emission probabilities, optional
    compo :: V.Vector Double,
    -- HMMER3 nodes
    nodes :: V.Vector NhmmerNode
  }
  deriving (Show, Eq)
  
-- | Data structure for the HMMER3 node
data HMMER3Node = HMMER3Node
  {
    nodeNumber :: Int,
    -- Match emission score, one per symbol in the alphabet
    matchEmissions :: V.Vector Double,
    -- map annotation - number of the alignment column
    nma :: Maybe Int,
    -- consensus residue 
    ncs :: Maybe Char,
    -- reference annotation
    nra :: Maybe Char,
    -- mask value - indicating if this position was masked during model construction
    nmv :: Bool,
    -- consensus structure
    ncs :: Maybe Char,
    -- insert emission score, one per symbol in the alphabet
    insertEmissions :: V.Vector Double,
    --  Transistion scores, m->m m->i m->d i->m i->i d->m d->d
    transitions :: V.Vector Double
  }
  deriving (Show, Eq)
