module Data.Choice.Proof

import Data.Choice.Types
import Data.Choice.Instances
import Data.Choice.Combinators
  
interface MonadChoice m => WfMonadChoice m where 
  lookGive : (a : m (Maybe (b, m b))) -> (Equal a ((look $ give {m = m} $ a) ))
