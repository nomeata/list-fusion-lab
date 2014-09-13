module ReportToMean where

import Criterion.Types
import Criterion.Analysis
import Statistics.Resampling.Bootstrap

reportToMean :: Report -> Double
reportToMean = estPoint . anMean . reportAnalysis

