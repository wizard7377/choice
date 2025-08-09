module Control.Monad.Alternative.Choice.Proof

import Control.Monad.Alternative.Choice.Types
import Control.Monad.Alternative.Choice.Instances
import Control.Monad.Alternative.Choice.Combinators
  
public export 
listLike : Choice a -> List a -> Type
listLike c l = (consumeChoiceT c = l)


singleChoice : {t : Type} -> (e : t) -> (listLike (pure e) [e])
singleChoice e = ?_
