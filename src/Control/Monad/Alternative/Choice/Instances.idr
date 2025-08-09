module Control.Monad.Alternative.Choice.Instances 
import Control.Monad.Alternative.Choice.Types 
import Control.Monad.Alternative.Choice.Internal
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Alternative.Class
Monad m => Cast (ChoiceT1 m a) (ChoiceT0 m a) where
  cast = map Just
Monad m => Cast (ChoiceT0 m a) (ChoiceT m a) where 
    cast = MkChoiceT
Monad m => Cast (ChoiceT1 m a) (ChoiceT m a) where 
    cast = MkChoiceT . map Just



