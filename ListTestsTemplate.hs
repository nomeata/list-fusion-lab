{-# LANGUAGE CPP #-}

#ifndef ITERATIONS
#define ITERATIONS 1000
#endif

module Main where

import           Benchmarks
import           Criterion
import           Criterion.Main
import           Criterion.Types
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    reports <- mapM go
        [ nf mapF ITERATIONS
        , nf mapNF ITERATIONS
        , nf appendF ITERATIONS
        , nf appendNF ITERATIONS
        -- , nf sumConcatInits ITERATIONS
        ]
    B.putStr (encode reports)
  where
    go = benchmarkWith' defaultConfig
        { verbosity = Quiet
        }
