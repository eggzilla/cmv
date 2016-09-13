-- | Datastructures for nHMMER hidden Markov models, representing RNA families
--   For more information on nHMMER consult <http://eddylab.org/infernal/>

module Bio.HMMData where

import qualified Data.Vector as V

-- | Data structure for HMMER
data HMMER3 = HMMER3
  {
    -- File format version; mandatory
    version :: String,
    -- Model name; mandatory
    name :: String,
    -- Accession number; optional
    acc :: String,
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
    cs :: Bool,
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
    -- Pfam model construction; optional
    bm :: Maybe String,
    -- Pfam full alignment search parameters; optional
    sm :: Maybe String,
    -- stats for E-value calculation describing location and slope of distribution; optional
    -- MSV - Gumpel distribution; optional
    localmsv :: (Double,Double),
    -- Viterbi - Gumpel distribution; optional
    localviterbi :: (Double,Double),
    -- Forward - exponential tail fitting; optional
    localforward :: (Double,Double),
    -- Symbol characters for this alphabet; mandatory
    hmm :: String,
    -- Models overall (composite) match state emission probabilities, optional
    compo ::  HMMER3Composite,
    -- HMMER3 nodes
    nodes :: V.Vector HMMER3Node
  }
  deriving (Show, Eq)
  
-- | Data structure for the HMMER3 node
data HMMER3Node = HMMER3Node
  {
    nodeId :: Int,
    -- Match emission score, one per symbol in the alphabet
    matchEmissions :: V.Vector Double,
    -- map annotation - number of the alignment column
    nma :: Maybe Int,
    -- consensus residue 
    ncr :: Maybe Char,
    -- reference annotation
    nra :: Maybe Char,
    -- mask value - indicating if this position was masked during model construction
    nmv :: Bool,
    -- consensus structure
    ncs :: Maybe Char,
    -- insert emission score, one per symbol in the alphabet
    insertEmissions :: V.Vector Double,
    -- Transistion scores, m->m m->i m->d i->m i->i d->m d->d
    m2m :: Maybe Double, 
    m2i :: Maybe Double,
    m2d :: Maybe Double,
    i2m :: Maybe Double,
    i2i :: Maybe Double,
    d2m :: Maybe Double,
    d2d :: Maybe Double
  }
  deriving (Show, Eq)

-- | Data structure for the HMMER3 overall probabilities
data HMMER3Composite = HMMER3Composite
  {
    -- Match emission score, one per symbol in the alphabet
    compositeMatchEmissions :: V.Vector Double,
    -- insert emission score, one per symbol in the alphabet
    compositeInsertEmissions :: V.Vector Double,
    --  Transistion scores, m->m m->i m->d i->m i->i d->m d->d
    cm2m :: Maybe Double, 
    cm2i :: Maybe Double,
    cm2d :: Maybe Double,
    ci2m :: Maybe Double,
    ci2i :: Maybe Double,
    cd2m :: Maybe Double,
    cd2d :: Maybe Double
  }
  deriving (Show, Eq)
