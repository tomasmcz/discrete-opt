# Discrete optimization

[![Build Status](https://secure.travis-ci.org/tomasmcz/discrete-opt.png?branch=master)](http://travis-ci.org/tomasmcz/discrete-opt)

## Work in progress

This is a repository of old Haskell code that will be transformed into a
discrete optimization library. It contains parts of ACO and SA solvers for TSP
and VRP as well as some heuristics (NN, 2-opt). 

## Documentation

http://tomasmcz.github.io/discrete-opt [Not quite complete yet.]

## Installation

I recommend using Cabal with a sandbox:

    cabal sandbox init
    cabal install --only-dep

You can test the program on a simple problem:    

    cabal run tsp aco data/tsp_51 -- --coords

To see available command-line options, you can run:

    cabal run -- -h

## License

This work is licensed under BSD-3 license.

The test data (in directory `data/TSPLIB95`) were obtained from
[TSPLIB95](http://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/).
Test files in directory `data` are just TSPLIB problems in a simplified format.
