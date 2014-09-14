list-fusion-lab
===============


This repositories contains
 * multiple implementations of the standard list-related functions, from
   `GHC.Base`, `GHC.List` and `Data.List`,
 * a set of benchmarks against these functions and
 * a driver to evaluate the various implementations, possibly against different
   versions of GHC.

Usage
-----

First, install the dependencies according to `list-fusion-lab.cabal`; these are
mostly for the driver.

Then, run a command line akin to

    list-fusion-lab -n 100 -m ListImpls.BaseFrom76,ListImpls.System --criterion

in the current directory to run the test suite.

The flag `-n` specifies the size of the lists to measure.

With `--criterion`, the criterion library is used to measure the benchmarks.
This is much more accurate, but requires criterion to be installed for the
compiler under evaulation, which might be tricky in the case of GHC HEAD.


TODO
----

 * A better `MeasureNaive.hs`.
 * Collecting allocation numbers (requires passing `+RTS -t` to the program and parsing the result)
 * More benchmarks
