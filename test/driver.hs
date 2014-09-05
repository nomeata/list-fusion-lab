{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Criterion.Types
import Data.Aeson
import Data.Data
import Data.Maybe
import Data.Text
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding
import Data.Typeable
import Filesystem.Path.CurrentOS hiding (decode)
import Options.Applicative hiding (Success, (&))
import Prelude hiding (FilePath)
import Shelly.Lifted hiding ((</>), find, trace)
import System.Environment
import System.IO hiding (FilePath)
import System.IO.Temp

data Options = Options
    { verbose      :: Bool
    , ghcPath      :: String
    , moduleName   :: String
    , fileTemplate :: String
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
    <*> strOption
        (   short 'f'
         <> long "file"
         <> value "test/ListTestsTemplate.hs"
         <> help "Path to test driver template")
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
        mods = splitOn "," (pack (fileTemplate opts))

    reports <- forM (cproduct ghcs mods) $ \(ghc, modName) -> shelly $ do
        run_ (fromText ghc)
            [ "-o", pack exePath
            , "-DDATA_LIST=" <> modName
            , pack (fileTemplate opts)
            ]
        -- jww (2014-09-05): Expect a list of CSV results
        output <- run (decodeString exePath) []
        let reports = decode (encodeUtf8 (fromStrict output)) :: Maybe [Report]
        return (ghc, modName, reports)

    print reports
  where
    cproduct xs ys = [ (x, y) | x <- xs, y <- ys ]
