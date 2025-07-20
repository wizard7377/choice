module Test.Choice

import Data.Choice
  
public export
Choice' : Type -> Type
Choice' t = ChoiceT IO t

