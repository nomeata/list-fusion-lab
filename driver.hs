{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.Binary
import Data.Data
import Data.Maybe
import Data.Text as T hiding (map, maximum, find, zipWith, init, last, concat, transpose)
import Data.List
import Data.Text.IO
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.RealFloat
import Control.Arrow (first, second)
import qualified Data.Map as M
import Data.Text.Encoding
import Filesystem.Path.CurrentOS hiding (decode, concat)
import Options.Applicative hiding (Success, (&))
import Prelude hiding (FilePath, readFile, putStrLn, putStr, replicate, unlines)
import Shelly.Lifted hiding ((</>), find, trace)
import System.IO hiding (FilePath, readFile, putStrLn, putStr)
import System.IO.Temp
import Data.Monoid

import ReportToMean

data Options = Options
    { verbose      :: Bool
    , useCriterion :: Bool
    , ghcPath      :: String
    , moduleName   :: String
    , runQuantity  :: Int
    , cliArgs      :: [String]
    }
    deriving (Data, Typeable, Show, Eq)

driverOpts :: Parser Options
driverOpts = Options
    <$> switch
        (   short 'v'
         <> long "verbose"
         <> help "Report progress verbosely")
    <*> switch
        (   long "criterion"
         <> help "use criterion")
    <*> strOption
        (   short 'G'
         <> long "ghc"
         <> value "ghc"
         <> help "Path to GHC executable")
    <*> strOption
        (   short 'm'
         <> long "module"
         <> value "ListImpls.System"
         <> help "Name of test module to import")
    <*> option
        (   short 'n'
         <> long "number"
         <> value 1000
         <> help "Number of iterations to run")
    <*> many (argument Just (metavar "ARGS"))

driverSummary :: String
driverSummary = "The list-fusion-lab"

main :: IO ()
main = do
    opts <- execParser optsDef
    runTest opts
  where
    optsDef = info
        (helper <*> driverOpts)
        (fullDesc <> progDesc "" <> header driverSummary)

{-
reportToResult :: Configuration -> Report -> Result
reportToResult conf rep = (pack (reportName rep), (conf, reportToMean rep))

reportsToFile :: [Report] -> IO ()
reportsToFile reports = do
    tmplDir <- getTemplateDir
    let tmplPath = decodeString tmplDir </> "default.tpl"
    tmpl <- readFile (encodeString tmplPath)
    html <- formatReport reports tmpl
    TL.writeFile "report.html" html
-}

type Configuration = (GHC, Module)
type Result = (BenchName, (Configuration, Double))
type GHC = Text
type BenchName = Text
type Module = Text

scaledShow :: [Double] -> Double -> Text
scaledShow list = \v -> showDouble (v/factor) <> singleton ' ' <> unit
  where
    -- is it really that complicated?
    showDouble = toStrict . toLazyText . formatRealFloat Fixed (Just 3)
    max = maximum list
    (unit, factor) =
        fromMaybe ("s", 1) $
        find (\(u,s) -> max < 1000 * s)
        [ ("ns", 1e-9)
        , ("µs", 1e-6)
        , ("ms", 1e-3)
        ]



renderResults :: [Result] -> IO ()
renderResults results = forM_ (groupEqual results) $ \(b, rs) -> do
    let printer = scaledShow (map snd rs)
    putStrLn $ "Benchmark " <> b <> ":"
    let rows = [showConf c <> " " <> printer v | (c,v) <- rs]
    putStr (alignAt " " rows)


showConf :: Configuration -> Text
showConf (g,m) = m <> "/" <> g

runTest :: Options -> IO ()
runTest opts = withSystemTempFile "listlab-exe" $ \exePath h -> do
    hClose h

    let ghcs = splitOn "," (pack (ghcPath opts))
        mods = splitOn "," (pack (moduleName opts))

    reports <- forM (cproduct ghcs mods) $ \conf@(ghc, modName) -> do
        reps <- shelly $ silently $ do
            let exe = pack exePath
            run_ (fromText ghc)
                [ "-o", exe
                , "-O"
                , "-fforce-recomp"
                , "-DDATA_LIST=" <> modName
                , "-DITERATIONS=" <> pack (show (runQuantity opts))
                , "-DMEASURE=" <> (if useCriterion opts
                                   then "MeasureCriterion"
                                   else "MeasureNaive")
                , "ListTestsTemplate.hs"
                ]
            echo $ "Collecting data from: " <> showConf conf
            run_ (fromText exe) []
            liftIO $ decodeFile "data"
        return $ map (\(n,r) -> (decodeUtf8 n, (conf, r))) reps
    renderResults (Prelude.concat reports)

  where
    cproduct xs ys = [ (x, y) | x <- xs, y <- ys ]

-- Nice generic functions copied from elsewhere
groupEqual :: Ord a => [(a,b)] -> [(a, [b])]
groupEqual xs = M.toList (M.fromListWith (++) (map (second ((:[]))) xs))


alignAt :: Text -> [Text] -> Text
alignAt d lines = T.unlines (map expands rows)
  where rows = map (splitOn d) lines
        widths = [map T.length r ++ repeat 0 | r <- rows]
        colwidths = map maximum (transpose widths)
        expand n s = s <> T.replicate (n - T.length s) (singleton ' ')
        expands [] = ""
        expands r = T.intercalate d $ zipWith expand colwidths (init r) ++ [last r]

