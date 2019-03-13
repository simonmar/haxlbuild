{-# LANGUAGE RecordWildCards #-}

module Haxl.Build.Types
  ( BuildState(..)
  , newBuildState
  , Haxl
  , Build
  , msg
  , addDep
  ) where

import Control.Monad
import Data.IORef
import GHC.Fingerprint

import Haxl.Core
import Haxl.Core.Monad

data BuildState = BuildState
  { depsRef :: IORef [Fingerprint]
  , checkingDependencies :: Maybe FilePath
    -- ^ @Just t@ if we're building a target, and the target exists
    -- with modification time @t@.  If a dependency has a modification
    -- time later than @t@, we should throw an exception to trigger a
    -- rebuild.
  , verbose :: Int
  }

newBuildState
  :: Int
  -> IO BuildState
newBuildState verb = do
  depsRef <- newIORef []
  return BuildState
    { checkingDependencies = Nothing
    , verbose = verb
    , ..}

type Haxl a = GenHaxl BuildState a

-- |
-- The build system monad.  The two principle operations of 'Build'
-- are 'need' and 'buildCmd'.  The 'Build' monad runs operations in
-- parallel when they are built using 'Applicative', and it can be
-- used with the 'ApplicativeDo' GHC extension to autoatically
-- parallelise 'do' expressions.
--
type Build a = GenHaxl BuildState a

msg :: Int -> String -> Build ()
msg n str = do
  v <- verbose <$> env userEnv
  when (v >= n) $ unsafeLiftIO $ putStrLn str

addDep :: Fingerprint -> Build ()
addDep hash = do
  ref <- depsRef <$> env userEnv
  unsafeLiftIO $ modifyIORef' ref (hash:)
