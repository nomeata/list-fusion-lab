{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Criterion.Main
import Criterion.Monad
import Criterion.Report
import Criterion.Types
import Data.Aeson
import Data.Data
import Data.Maybe
import Data.Text
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding
import Filesystem.Path.CurrentOS hiding (decode)
import Options.Applicative hiding (Success, (&))
import Prelude hiding (FilePath)
import Shelly.Lifted hiding ((</>), find, trace)
import System.IO hiding (FilePath)
import System.IO.Temp

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
         <> value "Data.List"
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

runTest :: Options -> IO ()
runTest opts = withSystemTempFile "listlab-exe" $ \exePath h -> do
    hClose h

    let ghcs = splitOn "," (pack (ghcPath opts))
        mods = splitOn "," (pack (moduleName opts))

    reports <- forM (cproduct ghcs mods) $ \(ghc, modName) -> shelly $ do
        run_ (fromText ghc)
            [ "-o", pack exePath
            , "-DDATA_LIST=" <> modName
            , "-DITERATIONS=" <> pack (show (runQuantity opts))
            , "ListTestsTemplate.hs"
            ]
        liftIO $ threadDelay 1000000
        output <- run (decodeString exePath) []
        let reports = decode (encodeUtf8 (fromStrict output)) :: Maybe [Report]
        return (ghc, modName, reports)

    withConfig defaultConfig
        { reportFile = Just "report.html"
        }
        $ report $ Prelude.concat [ z | (_, _, Just z) <- reports ]
  where
    cproduct xs ys = [ (x, y) | x <- xs, y <- ys ]
