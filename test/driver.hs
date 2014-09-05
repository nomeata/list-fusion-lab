{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
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

    forM_ (cproduct ghcs mods) $ \(ghc, mod) -> shelly $ do
        run_ (fromText ghc)
            [ "-o", pack exePath
            , pack ("-DDATA_LIST=" ++ moduleName opts)
            , pack (fileTemplate opts)
            ]
        -- jww (2014-09-05): Expect a list of CSV results
        _ <- run (decodeString exePath) []
        return ()
  where
    cproduct xs ys = [ (x, y) | x <- xs, y <- ys ]
