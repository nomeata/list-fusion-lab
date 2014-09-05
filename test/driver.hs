{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Logging
import Control.Monad.Logger (LogLevel(..))
import Data.Data
import Data.Text
import Data.Typeable
import Filesystem.Path.CurrentOS
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
main = withStdoutLogging $ do
    opts <- execParser optsDef

    setLogLevel $ if verbose opts then LevelDebug else LevelInfo
    setLogTimeFormat "%H:%M:%S"

    runTest opts
  where
    optsDef = info
        (helper <*> driverOpts)
        (fullDesc <> progDesc "" <> header driverSummary)

runTest :: Options -> IO ()
runTest opts = withSystemTempFile "listlab-exe" $ \exePath h -> do
    hClose h

    let ghc = ghcPath opts
    shelly $ do
        run_ (decodeString ghc)
            [ "-o", pack exePath
            , pack ("-DDATA_LIST=" ++ moduleName opts)
            , pack (fileTemplate opts)
            ]
        -- jww (2014-09-05): Expect a list of CSV results
        _ <- run (decodeString exePath) []
        return ()
