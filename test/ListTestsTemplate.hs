{-# LANGUAGE CPP #-}

#ifndef DATA_LIST
#define DATA_LIST Data.List
#endif

module Main where

import           Criterion
import           DATA_LIST
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

main = do
    report <- benchmark' $ nf (take 1000 . repeat) ()
    B.putStrLn $ encode report
