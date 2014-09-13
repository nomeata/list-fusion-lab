module MeasureCriterion where

import Criterion
import Criterion.Main
import Criterion.Types
import Data.Functor
import Control.DeepSeq

import ReportToMean

measure :: NFData b => (Int -> b) -> Int -> IO Double
measure f i = reportToMean <$> benchmarkWith' defaultConfig {verbosity = Quiet} (nf f i)

