||| The List Monad Transformer 
||| The List Monad Transformer allows for nondeterminsm with global state (i.e, free *choice*) 
module Data.Choice

import public Data.Choice.Alt.Types
import public Data.Choice.Alt.Instances as Instances
import public Data.Choice.Alt.Combinators
import public Data.Choice.Class
%hide Data.Choice.Alt.Types.MkChoiceT