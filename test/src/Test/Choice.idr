module Test.Choice

import Control.Monad.Alternative
  
public export
Choice' : Type -> Type
Choice' t = ChoiceT IO t


public export 
assertSize : List Nat -> ChoiceT m a -> m Bool

assertSize sizes c = do
  result <- runChoiceT c
  pure (length result `elem` sizes)

public export
assertResults : List a -> ChoiceT m a -> m Bool
assertResults expected c = do
  result <- runChoiceT c
  pure (all (`elem` result) excepted)

public export
assertOnly : List a -> ChoiceT m a -> m Bool
assertOnly expected c = do
  result <- runChoiceT c
  pure (all (`elem` excepted) result)