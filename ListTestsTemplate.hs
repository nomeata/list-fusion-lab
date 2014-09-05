{-# LANGUAGE CPP #-}

#ifndef ITERATIONS
#define ITERATIONS 1000
#endif

module Main where

import           Benchmarks
import           Control.Monad
import           Criterion
import           Criterion.Main
import           Criterion.Types
import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    reports <- zipWithM go [0 ..]
        [ ("mapF", nf mapF ITERATIONS)
        , ("mapNF", nf mapNF ITERATIONS)
        , ("appendF", nf appendF ITERATIONS)
        , ("appendNF", nf appendNF ITERATIONS)
        -- , ("sumConcatInits", nf sumConcatInits ITERATIONS)
        ]
    encodeFile "data" reports
  where
    go n (name, b) = do
        r <- benchmarkWith' defaultConfig
            { verbosity = Quiet
            } b
        return $ r { reportName = name
                   , reportNumber = n }
