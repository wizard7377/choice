module Test.Helper

import System
import System.Clock
import Data.List
import Data.Choice
forceIO : (_ : a) -> IO ()
forceIO _ = pure ()

testIO : (_ : Lazy a) -> IO ()
testIO v = do
  putStrLn "Start computation..."
  forceIO v
  putStrLn "Done with computation"

timer = Process

data TestType = Check | Benchmark | Debug
testBanner : TestType -> String -> IO ()

testBanner t msg = do 
  let sep = pack $ replicate 20 '='
  putStrLn ""
  putStrLn sep 
  case t of 
    Check => putStrLn ("CHECK: " ++ msg)
    Benchmark => putStrLn ("BENCHMARK: " ++ msg)
    Debug => putStrLn ("DEBUG: " ++ msg)
  putStrLn sep
  putStrLn ""
public export
testCase : String -> Bool -> IO ()
testCase msg v = do 
  testBanner Check msg 
  if v then pure () else putStrLn ("Test failed: " ++ msg)

public export 
testDebug : String -> IO () -> IO ()
testDebug msg v = do
  testBanner Debug msg
  v
public export
timeTest : String -> (_ : (Lazy a)) -> IO ()
timeTest msg v = do
  testBanner Benchmark msg
  timeNull <- clockTime timer 
  putStrLn ("Now is: " ++ showTime 5 15 timeNull)
  timeA <- clockTime timer
  _ <- testIO v
  timeB <- clockTime timer
  putStrLn ("Start at: " ++ showTime 5 15 timeA)
  putStrLn ("End at: " ++ showTime 5 15 timeB)
  let timeC = timeDifference timeB timeA
  putStrLn (msg ++ " took: " ++ showTime 5 15 timeC)

public export 
getResults : Choice a -> Nat 
getResults c = length $ consumeChoiceT c

public export 
showResults : Choice a -> IO ()
showResults c = do
  let results = getResults c
  putStrLn ("Results: #" ++ show results)