{-# LANGUAGE CPP #-}

#ifndef ITERATIONS
#define ITERATIONS 1000
#endif

#ifndef MEASURE
#define MEASURE MeasureCriterion
#endif

module Main where

import           Benchmarks
import           MEASURE

import           Control.Monad
import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    reports <- mapM go
        [ ("mapF", mapF )
        , ("mapNF", mapNF )
        , ("appendF", appendF )
        , ("appendNF", appendNF )
        -- , ("sumConcatInits", nf sumConcatInits ITERATIONS)
        ]
    encodeFile "data" (reports :: [(B.ByteString, Double)])
  where
    go (name, b) = do
        r <- measure b ITERATIONS
        return (B.pack name, r)
