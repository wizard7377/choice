module Test.Groups.Simple
  
import Control.Monad.Alternative
import Test.Helper
  
testSizeAt : Nat 
testSizeAt = 64
simpleTestA' : Nat -> Choice Nat
simpleTestA' n = do 
  x <- C.split $ pure [0..n]
  y <- C.split $ pure [0..n]
  z <- C.split $ pure [0..n]
  pure x <|> pure y <|> pure z <|> pure (x*y) <|> pure (y * z) <|> pure (x * z) <|> pure (x * y * z)
compareTestA' : Nat -> List Nat
compareTestA' n = do 
  x <- [0..n]
  y <- [0..n]
  z <- [0..n]
  pure x <|> pure y <|> pure z <|> pure (x*y) <|> pure (y * z) <|> pure (x * z) <|> pure (x * y * z)




simpleTestA : IO ()
simpleTestA = do 
  (timeTest "SimpleTestA" (simpleTestA' testSizeAt))
  --(testDebug "SimpleTestA" (showResults (simpleTestA' testSizeAt)))

compareTestA : IO ()
compareTestA = do
  (timeTest "Compare tests" (compareTestA' testSizeAt))
public export
simpleGroup : IO ()
simpleGroup = do 
  --compareTestA
  simpleTestA
