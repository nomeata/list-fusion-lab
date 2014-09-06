{-# LANGUAGE CPP #-}

#ifndef DATA_LIST
#define DATA_LIST ListImpls.System

#endif

module Benchmarks where

import DATA_LIST
import Prelude ((+), ($), Int, (.), sum, id)

mapF :: Int -> [Int]
mapF n    = map (+1) [0..n]

mapNF :: Int -> [Int]
mapNF n = map (+1) $ doNotFuse [0..n]

appendF :: Int -> [Int]
appendF n = [0..n] ++ [0..n]

appendNF :: Int -> [Int]
appendNF n = (doNotFuse [0..n]) ++ [0..n]

sumConcatInits :: Int -> Int
sumConcatInits n = sum . concat . inits $ [0..n]

-- Utils
doNotFuse = id
{-# NOINLINE [0] doNotFuse #-}

