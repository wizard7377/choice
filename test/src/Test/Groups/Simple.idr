module Test.Groups.Simple
  
import Data.Choice
import Test.Helper
  
simpleTestA' : Nat -> Choice Nat
simpleTestA' n = do 
  x <- C.split $ pure [0..n]
  y <- C.split $ pure [0..n]
  z <- C.split $ pure [0..n]
  pure x <|> pure y <|> pure z <|> pure (x*y) <|> pure (y * z) <|> pure (x * z) <|> pure (x * y * z)


public export 
simpleTestA : IO ()
simpleTestA = do 
  (timeTest "SimpleTestA" (simpleTestA' 100))
  (testDebug "SimpleTestA" (showResults (simpleTestA' 100)))
