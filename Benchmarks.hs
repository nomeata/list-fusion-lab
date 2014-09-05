{-# LANGUAGE CPP #-}

#ifndef DATA_LIST
#define DATA_LIST ListImpls.System

#endif

module Benchmarks where

import DATA_LIST

mapFusing :: Int -> [Int]
mapFusing n    = map (+1) [0..n]

mapNonFusing :: Int -> [Int]
mapNonFusing n = map (+1) $ doNotFuse [0..n]

-- Utils
doNotFuse = id
{-# NOINLINE [0] doNotFuse #-}

