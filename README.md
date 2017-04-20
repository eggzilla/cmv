![cmv](http://www.bioinf.uni-freiburg.de/~egg/cmvlogo.png "cmv") 
=========
*cmv* is a collection of tools four the visualisation of Hidden Markov Models (*hmmv*) and RNA-family models (*cmv*).
Moreover it can visualise comparisons of these models (*hmmcv*,*cmcv*), and annotate linked regions in the structural alignments they were constructed from and via, 3rd party tools, in their consensus secondary structure.

They are available as a commandline tools and as a webservice [cmvs](http://www.bioinf.uni-freiburg.de/~egg/cmvs).

The source code of *cmv* is open source and available via GitHub and Hackage (License GPL-3):

*   [![GitHub](https://img.shields.io/github/tag/eggzilla/cmv.svg)](https://github.com/eggzilla/cmv) [![Build Status](https://travis-ci.org/eggzilla/cmv.svg?branch=master)](https://travis-ci.org/eggzilla/cmv) [![Hackage](https://img.shields.io/hackage/v/cmv.svg)](https://hackage.haskell.org/package/cmv) [![Bioconda](https://anaconda.org/bioconda/cmv/badges/version.svg)](https://anaconda.org/bioconda/cmv)

For instruction how to use *cmv* please see the [Help page.](192.52.2.124/cmvs/help)

    ### <u>Optional Dependencies:</u>

    *   [R2R: inference of RNA alignments](http://infernal.janelia.org/)
    *   [forna](http://wash.github.io/rnacode/)
    
    ### <u>Installation via bioconda</u>

     cmv can be installed with all dependencies via [conda](https://conda.io/docs/install/quick.html). Once you have conda installed simply type:

         conda install -c bioconda cmv=1.0.0 

    ### <u>Installation via cabal-install</u>

    cmv is implemented in Haskell and can be installed via the Haskell package distribution sytem [cabal](https://www.haskell.org/cabal/). Once you have cabal installed simply type:

         cabal install cmv

   ### <u>Installation via stackage</u>

     cmv can also be install via the Haskell package distribution sytem [Stackage](https://www.stackage.org/), which guarantees consistent package builds. Once you have stackage installed simply type:

         stack install cmv


   ### <u>Precompiled Executables</u>

    *   Archlinux (ghc-8.0.1) [cmv 1.1.0 x86_64](http://www.bioinf.uni-freiburg.de/~egg/cmv/cmv-1.0.0)
