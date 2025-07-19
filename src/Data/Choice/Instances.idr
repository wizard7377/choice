module Data.Choice.Instances 
import Data.Choice.Types
import Data.Choice.Internal
import Control.Monad.Trans
--%default total
Monad f => Functor (ChoiceT f) where 
  map f (MkChoiceT c) = MkChoiceT $ mapMStep f <$> c

Monad m => Applicative (ChoiceT m) where 
  pure = MkChoiceT . pure . pureMStep
  (MkChoiceT f) <*> (MkChoiceT c) = MkChoiceT $ appMStep <$> f <*> c 
  
partial
Monad m => Monad (ChoiceT m) where
   (>>=) = flip bindChoiceT
   join = joinChoiceT
  
  
MonadTrans ChoiceT where 
  lift = liftChoiceT
  
  
(Monad m, Semigroup a) => Semigroup (ChoiceT m a) where 
  a <+> b = (<+>) <$> a <*> b
  
(Monad m, Monoid a) => Monoid (ChoiceT m a) where 
  neutral = pure neutral
  
 
(Monad m) => Alternative (ChoiceT m) where 
  empty = MkChoiceT $ pure MNil
  a <|> b = appendChoiceT a b
  
(Foldable m, Monad m) => Foldable (ChoiceT m) where
  foldr = foldrChoiceT

(Traversable m, Monad m) => Traversable (ChoiceT m) where 
  traverse = traverseChoiceT
[ChoiceAlt] MonadChoice m => Alternative m where 
  empty = give $ pure Nothing
  xl <|> yl = case (!(look xl)) of 
    Nothing => yl
    Just (x, xs) => give $ pure (Just (x, xs <|> yl))


private
lookChoice' : (Applicative (MStep m), Applicative m, Monad m) => MList m a -> Yield (MList m) a
lookChoice' v = case !v of 
  MNil => pure $ pure $ Nothing
  MCons x xs => pure $ pure $ Just (x, xs)

private 
giveChoice' : (Applicative (MStep m), Applicative m, Monad m) => Yield (MList m) a -> MList m a
giveChoice' v = do 
  v' <- v
  case v' of 
    MNil => pure MNil
    MCons Nothing ys => ?h0
    MCons (Just (x, xs)) ys => ?h1

Monad m => MonadChoice (ChoiceT m) where 
  look (MkChoiceT a) = ?h2
  give = ?h3
