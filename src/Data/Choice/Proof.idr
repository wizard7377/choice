module Data.Choice.Proof

import Data.Choice.Types
import Data.Choice.Instances
import Data.Choice.Combinators
  
public export 
listLike : Choice a -> List a -> Type
listLike c l = (consumeChoiceT c = l)


singleChoice : {t : Type} -> (e : t) -> (listLike (pure e) [e])
singleChoice e = ?_
