{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NondecreasingIndentation #-}

module Haxl.DataSource.Cmd
  ( cmd
  , CommandFailed(..)
  , initDataSource
  , RunCmdReq(..)
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception hiding (throw)
import Control.Monad (zipWithM, forever)
import Data.Hashable
import Data.Maybe
import Data.Time
import Data.Typeable
import GHC.Fingerprint
import System.Exit
import System.IO
import System.Process
import Text.Printf

import Haxl.Core

import Haxl.Build.Types

-- -----------------------------------------------------------------------------
-- External API

cmd :: FilePath -> [String] -> Haxl ()
cmd prog args = do
  let req = RunCmdReq prog args
  addDep (fingerprintString (unwords (prog:args)))
  e <- Haxl.Core.env id
  if isJust (checkingDependencies (userEnv e)) then return () else do
  exitCode <- dataFetch req
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure _ -> throw (CommandFailed prog args exitCode)


-- -----------------------------------------------------------------------------
-- Requests

data RunCmdReq a where
  RunCmdReq :: String -> [String] -> RunCmdReq ExitCode
  deriving Typeable -- requests must be Typeable

deriving instance Eq (RunCmdReq a)
deriving instance Show (RunCmdReq a)

deriving instance Read (RunCmdReq ExitCode)

instance ShowP RunCmdReq where showp = show

instance Hashable (RunCmdReq a) where
   hashWithSalt s (RunCmdReq prog args) = hashWithSalt s (0::Int,prog,args)


-- -----------------------------------------------------------------------------
-- Data source implementation

instance StateKey RunCmdReq where
  data State RunCmdReq = RunCmdState { logChan :: Chan String }

instance DataSourceName RunCmdReq where
  dataSourceName _ = "RunCmd"

instance DataSource u RunCmdReq where
  fetch = buildCmdFetch

initDataSource :: IO (State RunCmdReq)
initDataSource = do
  chan <- newChan
  forkIO (forever $ readChan chan >>= putStr)
  return RunCmdState { logChan = chan }

buildCmdFetch :: State RunCmdReq             -- current state
             -> Flags                        -- tracing verbosity, etc.
             -> u                            -- user environment
             -> PerformFetch RunCmdReq      -- tells the framework how to fetch

buildCmdFetch RunCmdState { logChan = chan } _flags _user =
  BackgroundFetch $ \bfs -> do
    t <- getCurrentTime
    zipWithM (\ n b -> async (fetch1 t chan n b)) [1..] bfs
    return ()

fetch1 :: UTCTime -> Chan String -> Int -> BlockedFetch RunCmdReq -> IO ()
fetch1 t0 chan n (BlockedFetch (RunCmdReq prog args) rvar) = do
  writeChan chan $ printf "[%d] %s\n" n (unwords (prog:args))
  r <- Control.Exception.try $
         withCreateProcess_ (proc prog args) { delegate_ctlc = True } $
            \_ _ _ p -> waitForProcess p
  t1 <- getCurrentTime
  let t = realToFrac (diffUTCTime t1 t0) :: Double
  let status = case r of
                 Right ExitSuccess -> "OK"
                 Right (ExitFailure n) -> "exit(" ++ show n ++ ")"
                 Left e -> show e
  writeChan chan $ printf "[%d] %s %.2fs\n" n status t
  putResult rvar r


-- Copied from System.Process, because it doesn't export withCreateProcess

withCreateProcess_
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ c action =
    bracketOnError (createProcess c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)


-- -----------------------------------------------------------------------------
-- Exceptions

data CommandFailed = CommandFailed String [String] ExitCode
  deriving (Typeable, Show)

instance Exception CommandFailed where
  toException = logicErrorToException
  fromException = logicErrorFromException

