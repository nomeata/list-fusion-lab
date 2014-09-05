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
        [ mapFusing ITERATIONS
        , mapNonFusing ITERATIONS
        ]
    B.putStrLn $ encode reports