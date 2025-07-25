module Data.Choice.Instances 
import Data.Choice.Types 
import Data.Choice.Internal
import Control.Monad.Trans
import Control.Monad.State
--%default total
  
public export
Monad f => Functor (ChoiceT f) where 
  map f (MkChoiceT c) = MkChoiceT $ do 
    c' <- c
    case c' of 
      Nothing => pure Nothing 
      Just v => pure $ Just $ mapStepT f v

public export
Monad m => Applicative (ChoiceT m) where 
  pure = MkChoiceT . pure . Just . pureStepT
  (MkChoiceT f) <*> (MkChoiceT c) = MkChoiceT $ do 
    f' <- f 
    c' <- c 
    case f' of 
      Nothing => pure Nothing
      Just f'' => case c' of 
        Nothing => pure Nothing
        Just c'' => pure $ Just $ appStepT f'' c'' 
  
public export
partial
Monad m => Monad (ChoiceT m) where
   (>>=) = flip bindChoiceT
   join = joinChoiceT
  
  
public export
Monad m => Functor (StepT m) where 
  map f c = mapStepT f c
public export
Monad m => Applicative (StepT m) where 
  pure = pureStepT
  f <*> c = appStepT f c
public export
Monad m => Functor (ListT m) where 
  map f c =  
    case !c of 
      Nothing => pure $ Nothing
      Just v => pure $ Just $ f <$> v
public export
Monad m => Functor (ListT m) => Applicative (ListT m) where 
  pure = pure . Just . pure
  (<*>) = appListT
public export
MonadTrans ChoiceT where 
  lift = liftChoiceT
  
public export 
(Monad m, Semigroup a) => Semigroup (ChoiceT m a) where 
  a <+> b = (<+>) <$> a <*> b
public export  
(Monad m, Monoid a) => Monoid (ChoiceT m a) where 
  neutral = pure neutral
  
 
public export
(Monad m) => Alternative (ChoiceT m) where 
  empty = MkChoiceT $ pure $ Nothing
  a <|> b = appendChoiceT a b
  
public export
(Foldable m, Monad m) => Foldable (ChoiceT m) where
  foldr = foldrChoiceT

public export
(Traversable m, Monad m) => Traversable (ChoiceT m) where 
  traverse = traverseChoiceT
  
%defaulthint
public export
[ChoiceAlt] MonadChoice m => Alternative m where 
  empty = give $ pure Nothing
  xl <|> yl = case (!(look xl)) of 
    Nothing => yl
    Just (x, xs) => give $ pure (Just (x, xs <|> yl))



public export
Monad m => MonadChoice (ChoiceT m) where 
  look = lookChoice
  give = giveChoice


covering
public export
Monad m => Bifunctor (\a => \b => ChoiceT m (Either a b)) where
  bimap f g = map (bimap f g)

MonadState s m => MonadState s (ChoiceT m) where 
  get = lift get
  put = lift . put




