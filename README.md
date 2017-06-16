![cmv](http://www.bioinf.uni-freiburg.de/~egg/cmvlogo.png "cmv") 
=========
*cmv* is a collection of tools for the visualisation of Hidden Markov Models (*HMMV*) and RNA-family models (*CMV*).
Moreover it can visualise comparisons of these models (*HMMCV*,*CMCV*), and annotate linked regions in the structural alignments they were constructed from and via, 3rd party tools, in their consensus secondary structure.

They are available as a commandline tools and as a webservice [cmvs](http://rna.informatik.uni-freiburg.de/CMVS/).

The source code of *cmv* is open source and available via GitHub and Hackage (License GPL-3):

*   [![GitHub](https://img.shields.io/github/tag/eggzilla/cmv.svg)](https://github.com/eggzilla/cmv) [![Build Status](https://travis-ci.org/eggzilla/cmv.svg?branch=master)](https://travis-ci.org/eggzilla/cmv) [![Hackage](https://img.shields.io/hackage/v/cmv.svg)](https://hackage.haskell.org/package/cmv) [![Bioconda](https://anaconda.org/bioconda/cmv/badges/version.svg)](https://anaconda.org/bioconda/cmv)

For instruction how to use *cmv* please see the [Help page.](192.52.2.124/cmvs/help)

   ### Optional Dependencies:
   *cmv* creates output which can be visualised via following optional dependencies.

    *   [R2R] (http://breaker.research.yale.edu/R2R/)
    *   [forna](http://rna.tbi.univie.ac.at/forna/)
    
   ### <u>Installation via bioconda</u>

   cmv can be installed with all dependencies via [conda](https://conda.io/docs/install/quick.html). Once you have conda installed simply type:

         conda install -c bioconda cmv=1.0.0 

   ### Installation via cabal-install

   cmv is implemented in Haskell and can be installed via the Haskell package distribution sytem [cabal](https://www.haskell.org/cabal/). Once you have cabal installed simply type:

         cabal install cmv

   ### Precompiled Executables

    *   Linux (ghc-8.0.1) [HMMV 1.0.0 x86_64](http://www.bioinf.uni-freiburg.de/~egg/cmvs/bin/HMMV)
    *   Linux (ghc-8.0.1) [HMMCV 1.0.0 x86_64](http://www.bioinf.uni-freiburg.de/~egg/cmvs/bin/HMMCV)
    *   Linux (ghc-8.0.1) [CMV 1.0.0 x86_64](http://www.bioinf.uni-freiburg.de/~egg/cmvs/bin/CMV)
    *   Linux (ghc-8.0.1) [CMCV 1.0.0 x86_64](http://www.bioinf.uni-freiburg.de/~egg/cmvs/bin/CMCV)
   
