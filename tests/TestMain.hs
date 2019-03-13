module Main where

import Prelude hiding (writeFile)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import System.Directory
import System.FilePath
import qualified System.IO
import System.IO.Temp
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit

import Haxl.Build
import Haxl.Build.Types
import Haxl.Core
import Haxl.Core.Monad (unsafeLiftIO) -- TODO expose this in Haxl.Build, or something

main :: IO ()
main = defaultMain $ hUnitTestToTests allTests


allTests :: Test
allTests = TestList
  [ TestLabel "build" buildTest
  ]


type Counter = IORef (Map String Int)
bumpCounter :: Counter -> String -> Build ()
bumpCounter r tag = ioWhenBuilding $ modifyIORef r (Map.insertWith (+) tag 1)

-- naughty primitive to detect when we're building things
ioWhenBuilding :: IO () -> Build ()
ioWhenBuilding act = do
  e <- Haxl.Core.env userEnv
  if isJust (checkingDependencies e) then return () else unsafeLiftIO act


buildTest :: Test
buildTest = TestCase $ withSystemTempDirectory "haxlbuild" $ \tmpdir -> do
  setCurrentDirectory tmpdir

  -- set up "source files"
  System.IO.writeFile "d" "d"

  -- test a basic build
  counter <- newIORef Map.empty
  runBuild 3 (buildA "" counter)
  result <- readFile "a"
  assertEqual "0 a" "bdcd" result
  c <- readIORef counter
  assertEqual "0 counter" (Map.fromList [("b",1),("c",1),("a",1)]) c

  -- building again shouldn't run the rule
  counter <- newIORef Map.empty
  runBuild 3 (buildA "" counter)
  c <- readIORef counter
  assertEqual "1 counter" Map.empty c

  -- removing the file shouldn't build anyting (it is now cached)
  removeFile "a"
  counter <- newIORef Map.empty
  runBuild 3 (buildA "" counter)
  result <- readFile "a"
  assertEqual "2 a" "bdcd" result
  c <- readIORef counter
  assertEqual "2 counter" Map.empty c

  -- modifying 'd' should build a,b,c
  System.IO.writeFile "d" "x"
  counter <- newIORef Map.empty
  runBuild 3 (buildA "" counter)
  result <- readFile "a"
  assertEqual "3 a" "bxcx" result
  c <- readIORef counter
  assertEqual "3 counter" (Map.fromList [("b",1),("c",1),("a",1)]) c

  -- reverting 'd' should bring things back from the cache without building anything
  System.IO.writeFile "d" "d"
  counter <- newIORef Map.empty
  runBuild 3 (buildA "" counter)
  result <- readFile "a"
  assertEqual "4 a" "bdcd" result
  c <- readIORef counter
  assertEqual "4 counter" Map.empty c

  -- modifying the rule for c should rebuild c and a only
  counter <- newIORef Map.empty
  runBuild 3 (buildA' "" counter)
  result <- readFile "a"
  assertEqual "5 a" "bdxd" result
  c <- readIORef counter
  assertEqual "5 counter" (Map.fromList [("c",1),("a",1)]) c

  -- make a bigger build system
  counter <- newIORef Map.empty
  createDirectoryIfMissing True "x"
  createDirectoryIfMissing True "y"
  System.IO.writeFile "x/d" "d"
  System.IO.writeFile "y/d" "d"
  runBuild 3 $ create "z" $ do
    fs <- sequence [buildA "x" counter, buildA "y" counter]
    srcs <- mapM readBuiltFile fs
    return (concat srcs)
  result <- readFile "z"
  assertEqual "6 z" "bdcdbdcd" result


{-
    a
   / \
  b   c
   \ /
    d  <-- source file
-}

buildA :: FilePath -> Counter -> Build File
buildA dir counter = create (dir </> "a") $ do
  bumpCounter counter (dir </> "a")
  fs <- sequence [buildB dir counter, buildC dir counter]
  srcs <- mapM readBuiltFile fs
  return (concat srcs)

buildB :: FilePath -> Counter -> Build File
buildB dir counter = create (dir </> "b") $ do
  dfile <- source (dir </> "d")
  d <- readBuiltFile dfile
  bumpCounter counter (dir </> "b")
  return ("b" ++ d)

buildC :: FilePath -> Counter -> Build File
buildC dir counter = create (dir </> "c") $ do
  dfile <- source (dir </> "d")
  d <- readBuiltFile dfile
  bumpCounter counter (dir </> "c")
  return ("c" ++ d)

-- Alternative build system with a modified rule for c

buildA' :: FilePath -> Counter -> Build File
buildA' dir counter = create (dir </> "a") $ do
  bumpCounter counter (dir </> "a")
  fs <- sequence [buildB dir counter, buildC' dir counter]
  srcs <- mapM readBuiltFile fs
  return (concat srcs)

buildC' :: FilePath -> Counter -> Build File
buildC' dir counter = create (dir </> "c") $ do
  dfile <- source (dir </> "d")
  d <- readBuiltFile dfile
  bumpCounter counter (dir </> "c")
  return ("x" ++ d)
