{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.Binary
import Data.Data
import Data.Maybe
import Data.Text hiding (map)
import Data.Text.IO
import Control.Arrow (first, second)
import qualified Data.Map as M
import Data.Text.Encoding
import Filesystem.Path.CurrentOS hiding (decode)
import Options.Applicative hiding (Success, (&))
import Prelude hiding (FilePath, readFile, putStrLn)
import Shelly.Lifted hiding ((</>), find, trace)
import System.IO hiding (FilePath, readFile, putStrLn)
import System.IO.Temp

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

renderResults :: [Result] -> IO ()
renderResults results = forM_ (groupEqual results) $ \(b, rs) -> do
    putStrLn $ "Benchmark " <> b <> ":"
    forM rs $ \(c,v) -> do
        putStrLn $ showConf c <> " " <> pack (show v)

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

groupEqual :: Ord a => [(a,b)] -> [(a, [b])]
groupEqual xs = M.toList (M.fromListWith (++) (map (second ((:[]))) xs))

