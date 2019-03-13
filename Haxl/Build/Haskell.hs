module Haxl.Build.Haskell
  ( haskellProgram
  , haskellObj
  , haskellHi
  , haskellDependencies
  ) where

import Control.Monad
import Haxl.Build
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath
import Data.List

-- -----------------------------------------------------------------------------
-- Compiling Haskell programs

haskellProgram :: FilePath -> [FilePath] -> Build File
haskellProgram prog hsfiles = build prog $ do
  deps <- haskellDependencies hsfiles "deps"
  _ <- mapM (haskellObj deps . dropExtensions) hsfiles
  cmd "ghc" (["-o", prog] ++ map (mkObj . dropExtensions) hsfiles)

type Module = String
type HaskellDeps = Map Module [Module]

haskellObj :: HaskellDeps -> Module -> Build File
haskellObj deps mod = do
 build obj $ do
  _ <- mapM (haskellHi deps) $ Map.findWithDefault [] mod deps
  cmd "ghc" ["-c", src, "-o", obj]
  void $ source src
 where
  src = mkHs mod; obj = mkObj mod

haskellHi :: HaskellDeps -> Module -> Build File
haskellHi deps mod = build (mkHi mod) $ void $ haskellObj deps mod

mkHs, mkObj, mkHi :: FilePath -> FilePath
mkHs = (<.> "hs")
mkObj = (<.> "o")
mkHi = (<.> "hi")

haskellDependencies :: [FilePath] -> FilePath -> Build (Map FilePath [FilePath])
haskellDependencies hsfiles depfile = do
  depsFile <- build depfile $ do
    cmd "ghc" (["-M", "-dep-makefile=" ++ depfile, "-dep-suffix", ""]
                ++ hsfiles)
    mapM_ source hsfiles
  deps <- readBuiltFile depsFile
  return (parseDepFile deps)

parseDepFile :: String -> Map Module [Module]
parseDepFile f = Map.fromListWith (++)
  [ (dropExtensions m, [dropExtensions dep])
  | [m,_,dep] <- map words (lines f)
  , ".o" `isSuffixOf` m
  , ".hi" `isSuffixOf` dep ]
