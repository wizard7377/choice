module Control.Monad.Alternative.Class

import Control.Monad.Morph
import Control.Monad.Trans
public export 
interface Alternative m => Monad m => MonadLogic m where 
  split : m a -> m (Maybe (a, m a))

public export 
interface Alternative m => MonadChoice m where 
  ro : m a -> m (Maybe (m a, m a)) 

  
