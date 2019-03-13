{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Monoid
import Haxl.Build
import Haxl.Build.Haskell
import Options.Applicative

data Options = Options
  { progName :: String
  , verbose :: Int
  , files :: [String]
  }

parseOptions :: Parser Options
parseOptions = do
  progName <- strOption
   ( short 'o'
   <> metavar "PROG"
   <> value "main"
   <> help "Output binary name (default \"main\")" )
  verbose <- option auto
    ( long "verbose"
   <> value 0
   <> short 'v'
   <> help "How verbose to be" )
  files <- many (argument str (metavar "FILE"))
  return Options{..}

main = do
  Options{..} <- execParser opts
  runBuild verbose $ haskellProgram progName files
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Build a Haskell program"
     <> header "hmake - build a Haskell program" )


