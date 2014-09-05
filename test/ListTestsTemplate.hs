module Main where

import           Benchmarks
import           Criterion
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

main = do
    reports <- mapM benchmark'
        [ mapFusing 1000
        , mapNonFusing 1000
        ]
    B.putStrLn $ encode reports
