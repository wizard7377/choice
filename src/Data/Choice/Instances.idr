module Data.Choice.Instances 
import Data.Choice.Types
import Data.Choice.Internal


Monad f => Functor (Choice f) where 
  map f (MkChoice c) = MkChoice $ mapMStep f <$> c

Monad m => Applicative (Choice m) where 
  pure = MkChoice . pure . pureMStep
  (MkChoice f) <*> (MkChoice c) = MkChoice $ appMStep <$> f <*> c 
  
Monad m => Monad (Choice m) where
  c >>= f = bindChoice f c
  join = ?j
  
