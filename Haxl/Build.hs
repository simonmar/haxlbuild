{- |
A Build System in <500 lines using Haxl, with

* arbitrary dynamic dependencies
* maximal parallelism (use mapM / sequence / <*> to parallelise)
* using file contents, not timestamps, to detect changes
* cached artifacts: rebuild to a previous state without executing commands
* zero state, except for the directory of cached artifacts
* using speculative execution to discover dependencies
* no global registry of rules
* detects when commands change and rebuild
* rules for Haskell are provided in Haxl.Build.Haskell

Rules of the game:

* Use 'source' to register a dependency on all source files that you
  use. If you forget this, then the build system won't rebuild when
  the source file changes.

* Use 'cmd' to run external commands. Commands should be
  deterministic, given the same input files and arguments.  A
  non-deterministic command won't be detected, it will just make the
  build system misbehave.

* Use 'create' and 'readBuiltFile' to write and read files
  respectively.

* Don't do 'build' multiple times on the same file with different
  build instructions.

* Don't use 'unsafeLiftIO' to do arbitrary I/O. You will probably end
  up doing I/O without registering a dependency and make a
  non-deterministic rules, which will cause the build system to
  misbehave. Any new I/O primitives must be added to the library
  or as Haxl datasources.
-}


{-
Compared with Shake:
- In Shake we can cache the dependencies instead of the commands.
  That's because Shake knows how to bring a dependency up to date,
  because it has the map from file to build rule.  In HaxlShake, we
  have to run the build rule to get the deps, so we have to cache the
  commands that were run.

- haxlbuild can easily tell if the build system was changed, because
  we hash the commands and treat those as dependencies.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Haxl.Build
  ( runBuild
  , build
  , create
  , source
  , cmd
  , Build
  , File
  , log
  , andThen
  , readBuiltFile
  ) where

{- TODO
 - unlinking the file before building is suboptimal in the case of GHC, we don't get to
   do recomp checking. How to fix this?
 - Need to reap the cache, perhaps have a maximum size
 - Print out stats at the end:
   - number of commands
   - number of needs
   - number of needs rebuilt (%)
 - can detect multiple different needs for same file?
-}

import Haxl.Prelude
import Prelude ()

import Control.Exception hiding (throw)
import Control.Monad hiding (sequence_, forM)
import Data.Either
import Data.Hashable
import Data.IORef
import Data.Maybe
import GHC.Fingerprint
import Haxl.Core
import Haxl.Core.Monad
import System.Directory
import System.FilePath
import qualified System.IO
import System.IO.Error
import System.Posix
import System.Process (callCommand)
import Text.Printf
import qualified Control.Exception as Exception

import Haxl.Build.Types
import Haxl.DataSource.Cmd


-- ---------------------------------------------------------------------------

-- |
-- Build a file.
--
-- * The file will only be built once per run.
--
-- * The file will be brought up to date, either by restoring its
--   contents from the cache if we have the correct version in the
--   cache, or by executing the supplied 'Build' computation to create
--   it otherwise.
--
build
   :: FilePath  -- ^ File name
   -> Build ()  -- ^ Computation that builds the file. It may call 'need'
                --   to build other files that this file depends on.
   -> Build File

build f doBuild = do
  buildFile f doBuild >>= addDep
  return (File f)

-- | An abstract type to enforce the invariant that only files that have
-- been built can be read.
newtype File = File FilePath

-- |
-- Create a file.
--
-- This is like 'build', but it is used when we want to create the
-- contents of the file in the build system rather than executing an
-- external command to create the file for us.
--
create
   :: FilePath  -- ^ File name
   -> Build String
   -> Build File
create f doBuild = build f (doBuild >>= writeFile f)
  where
  -- We must log a dependency on the contents of the file. Otherwise,
  -- two rules that write different contents but are otherwise
  -- identical would be considered to have the same fingerprint.
  writeFile f str = do
    addDep (fingerprintString str)
    e <- Haxl.Core.env userEnv
    if isJust (checkingDependencies e)
      then return ()
      else unsafeLiftIO $ System.IO.writeFile f str


-- | Record that a file is used by the current target, but it doesn't
-- need to be built. For example, we can use this to record a
-- dependency on source files.
--
source :: FilePath -> Build File
source f = do
  maybeHash <- unsafeLiftIO $ fingerprintFile f
  case maybeHash of
    Nothing -> throw (ErrorCall ("source file does not exist: " ++ f))
    Just hash -> addDep hash >> return (File f)

-- | read a file, enforcing the invariant that we can only read files
-- that we have built and therefore regisgtered a dependency on.
readBuiltFile :: File -> Build String
readBuiltFile (File f) = unsafeLiftIO $ readFile f

-- | forced sequencing in Haxl
andThen :: Haxl () -> Haxl b -> Haxl b
andThen a b = a >>= \_ -> b

-- | Run a build system
runBuild :: Int -> Build a -> IO a
runBuild verb doBuild = do
  createDirectoryIfMissing True artifactDir
  runCmdState <- initDataSource
  bstate <- newBuildState verb
  env <- initEnv (stateSet runCmdState stateEmpty) bstate
  runHaxl env $ doBuild


-- -----------------------------------------------------------------------------
-- Memoizing build steps

data BuildFile target result where
  BuildFile :: FilePath -> BuildFile FilePath Fingerprint

deriving instance Eq target => Eq (BuildFile target result)
deriving instance Show target => Show (BuildFile target result)

instance Hashable target => Hashable (BuildFile target result) where
  hashWithSalt salt (BuildFile file) = hashWithSalt salt file

--
-- | Cache the build of a particular target
--
buildFile :: FilePath -> Build () -> Haxl Fingerprint
buildFile f doBuild = cachedComputation (BuildFile f) $ do
  e <- env id
  let bs@BuildState{..} = userEnv e
  msg 1 $ printf "buildFile: checking dependencies of %s" f
  depsRef <- unsafeLiftIO $ newIORef []
  _ <- withEnv e { userEnv = bs { checkingDependencies = Just f, depsRef = depsRef } } doBuild
  hash <- fingerprintFingerprints . sort <$> unsafeLiftIO (readIORef depsRef)
    -- NB. sort the fingerprints of the deps to get a deterministic
    -- result, because execution order might vary depending on whether
    -- we're building for real or fetching from the cache.
  cacheID <- unsafeLiftIO $ maybeFileID (artifactDir </> show hash)
  if isNothing cacheID then do
    msg 1 $ printf "buildFile: building %s [%s]" f (show hash)
     -- unlink the file first, we don't want to overwrite the wrong cached file
    unsafeLiftIO $ tryJust (guard . isDoesNotExistError) $ removeFile f
    depsRef <- unsafeLiftIO $ newIORef []
    _ <- withEnv e { userEnv = bs { checkingDependencies = Nothing, depsRef = depsRef } }
           doBuild
    unsafeLiftIO $ saveArtifact f hash
  else do
    r <- unsafeLiftIO $ maybeFileID f
    case r of
      Just fid -> do
        if Just fid == cacheID then do
          msg 1 $ printf "buildFile: existing artifact is up to date for %s [%s]" f (show hash)
          return ()
        else do
          msg 1 $ printf "buildFile: updating artifact from cache for %s [%s]" f (show hash)
          unsafeLiftIO $ do
            removeFile f
            getArtifact f hash
            callCommand ("touch " ++ f)
          return ()
      Nothing -> do
        msg 1 $ printf "buildFile: using cached artifact for %s [%s]" f (show hash)
        unsafeLiftIO $ do
          getArtifact f hash
          callCommand ("touch " ++ f)
  return hash


fingerprintFile :: FilePath -> IO (Maybe Fingerprint)
fingerprintFile f = do
  r <- Exception.tryJust (Just . isDoesNotExistError) $ getFileHash f
  case r of
    Left _ -> return Nothing
    Right hash -> return (Just hash)

artifactDir :: FilePath
artifactDir = ".haxl-artifacts"

saveArtifact :: FilePath -> Fingerprint -> IO ()
saveArtifact file fingerprint =
  createLink file (artifactDir </> show fingerprint)

getArtifact :: FilePath -> Fingerprint -> IO Bool
getArtifact file fingerprint = do
  r <- tryJust (guard . isDoesNotExistError) $ createLink (artifactDir </> show fingerprint) file
  return (isRight r)

maybeFileID :: FilePath -> IO (Maybe FileID)
maybeFileID file = do
  r <- tryJust (guard . isDoesNotExistError) $ fileID <$> getFileStatus file
  case r of
    Left{} -> return Nothing
    Right f -> return (Just f)
