{-# LANGUAGE CPP #-}

#ifndef ITERATIONS
#define ITERATIONS 1000
#endif

module Main where

import           Benchmarks
import           Criterion
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

main = do
    reports <- mapM benchmark'
        [ nf mapF ITERATIONS
        , nf mapNF ITERATIONS
        , nf appendF ITERATIONS
        , nf appendNF ITERATIONS
        , nf sumConcatInits ITERATIONS
        ]
    B.putStrLn $ encode reports
