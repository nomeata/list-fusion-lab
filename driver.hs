{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Criterion.Main
import Criterion.Monad
import Criterion.Report
import Criterion.Types
import Data.Binary
import Data.Data
import Data.Maybe
import Data.Text hiding (map)
import Data.Text.IO
import Control.Arrow (second)
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy.Encoding
import Filesystem.Path.CurrentOS hiding (decode)
import Options.Applicative hiding (Success, (&))
import Prelude hiding (FilePath, readFile, putStrLn)
import Shelly.Lifted hiding ((</>), find, trace)
import System.IO hiding (FilePath, readFile, putStrLn)
import System.IO.Temp

import ReportToMean

data Options = Options
    { verbose      :: Bool
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

reportToResult :: Configuration -> Report -> Result
reportToResult conf rep = (pack (reportName rep), (conf, reportToMean rep))

reportsToFile :: [Report] -> IO ()
reportsToFile reports = do
    tmplDir <- getTemplateDir
    let tmplPath = decodeString tmplDir </> "default.tpl"
    tmpl <- readFile (encodeString tmplPath)
    html <- formatReport reports tmpl
    TL.writeFile "report.html" html

type Configuration = (GHC, Module)
type Result = (BenchName, (Configuration, Double))
type GHC = Text
type BenchName = Text
type Module = Text

renderOurReport :: [Result] -> IO ()
renderOurReport results = forM_ (groupEqual results) $ \(b, rs) -> do
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

    reports <- forM (cproduct ghcs mods) $ \(ghc, modName) -> do
        reps <- shelly $ silently $ do
            let exe = pack exePath
            run_ (fromText ghc)
                [ "-o", exe
                , "-O"
                , "-fforce-recomp"
                , "-DDATA_LIST=" <> modName
                , "-DITERATIONS=" <> pack (show (runQuantity opts))
                , "ListTestsTemplate.hs"
                ]
            echo $ "Collecting data from: " <> ghc <> " + " <> modName
            run_ (fromText exe) []
            liftIO $ decodeFile "data"
        return $ map (reportToResult (ghc, modName)) reps
    --reportsToFile (Prelude.concat reports)
    renderOurReport (Prelude.concat reports)

  where
    cproduct xs ys = [ (x, y) | x <- xs, y <- ys ]

groupEqual :: Ord a => [(a,b)] -> [(a, [b])]
groupEqual xs = M.toList (M.fromListWith (++) (map (second ((:[]))) xs))

