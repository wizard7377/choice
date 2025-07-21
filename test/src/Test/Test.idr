module Test.Test
import Test.Groups.Simple
public export
testMain : IO ()
testMain = do 
  simpleGroup 
  pure ()
