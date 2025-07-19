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
  
  
(Monad m, Semigroup a) => Semigroup (Choice m a) where 
  a <+> b = (<+>) <$> a <*> b
  
(Monad m, Monoid a) => Monoid (Choice m a) where 
  neutral = pure neutral
  
 
(Monad m) => Alternative (Choice m) where 
  empty = MkChoice $ pure MNil
  a <|> b = appendChoice a b
  
(Foldable m, Monad m) => Foldable (Choice m) where
  foldr f a (MkChoice c) = ?fold

(Traversable m, Monad m) => Traversable (Choice m) where 
  traverse f c = ?t
[choiceAlt] MonadChoice m => Alternative m where 
  empty = give $ pure Nothing
  xl <|> yl = case (!(look xl)) of 
    Nothing => yl
    Just (x, xs) => give $ pure (Just (x, xs <|> yl))

