module MeasureNaive where

import Control.DeepSeq
import Data.Time.Clock.POSIX

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

oneRun :: NFData b => (Int -> b) -> Int -> IO Double
oneRun f i = do
  before <- getTime
  f i `deepseq` return ()
  after <- getTime
  return (after - before)
{-# NOINLINE oneRun #-}

avg :: Int -> IO Double -> IO Double
avg n act = do
    ms <- sequence (replicate n act)
    return (sum ms / fromIntegral n)

measure :: NFData b => (Int -> b) -> Int -> IO Double
measure f i = do
  calibrate <- avg 10 $ oneRun (const ()) i
  measure   <- avg 100 $ oneRun f i
  return $ measure - calibrate

