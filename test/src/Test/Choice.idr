module Test.Choice

import Data.Choice
Choice' : Type -> Type
Choice' t = ChoiceT IO t

getSize : Choice' a -> IO Int
getSize x = ?h0 $ execChoice x
