||| The List Monad Transformer 
||| The List Monad Transformer allows for nondeterminsm with global state (i.e, free *choice*) 
module Control.Monad.Alternative

import public Control.Monad.Alternative.Choice.Types
import public Control.Monad.Alternative.Choice.Instances as Instances
import public Control.Monad.Alternative.Choice.Combinators
import public Control.Monad.Alternative.Class
%hide Control.Monad.Alternative.Choice.Types.MkChoiceT