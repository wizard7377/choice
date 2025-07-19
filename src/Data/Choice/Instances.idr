module Data.Choice.Instances 
import Data.Choice.Types
import Data.Choice.Internal
import Control.Monad.Trans
--%default total
Monad f => Functor (Choice f) where 
  map f (MkChoice c) = MkChoice $ mapMStep f <$> c

Monad m => Applicative (Choice m) where 
  pure = MkChoice . pure . pureMStep
  (MkChoice f) <*> (MkChoice c) = MkChoice $ appMStep <$> f <*> c 
  
partial
Monad m => Monad (Choice m) where
   (>>=) = flip bindChoice
   join = joinChoice
  
  
MonadTrans Choice where 
  lift = liftChoice
  
  
lookChoice : Monad m => (Choice m a) -> Yield (Choice m) a
lookChoice (MkChoice v) = MkChoice $ do 
  v' <- v 
  case v' of 
    MNil => pure MNil
    MCons x xs => 
      (\y0 => \y1 => Just (y0, y1)) x <$> xs
      
Monad m => MonadChoice (Choice m) where
  look = lookChoice
  give = ?g
