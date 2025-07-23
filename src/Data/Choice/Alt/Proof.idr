module Data.Choice.Alt.Proof

import Data.Choice.Alt.Types
import Data.Choice.Alt.Instances
import Data.Choice.Alt.Combinators
  
public export 
listLike : Choice a -> List a -> Type
listLike c l = (consumeChoiceT c = l)


singleChoice : {t : Type} -> (e : t) -> (listLike (pure e) [e])
singleChoice e = ?_
