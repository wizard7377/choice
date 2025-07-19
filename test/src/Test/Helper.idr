module Test.Helper
  

testCase : String -> Bool -> IO ()
testCase msg v = if v then pure () else putStrLn ("Test failed: " ++ msg)
