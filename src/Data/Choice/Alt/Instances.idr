module Data.Choice.Alt.Instances 
import Data.Choice.Alt.Types 
import Data.Choice.Alt.Internal
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Choice.Class
--%default total
  
yielding : Applicative m => Maybe a -> Yield m a
yielding (Just x) = YieldOne x
yielding Nothing = YieldZero
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
covering
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
  empty = give $ pure $ yielding Nothing
  xl <|> yl = case (!(look xl)) of 
    YieldZero => yl
    YieldOne x => give $ pure $ YieldMany (pure x) yl
    YieldMany x xs => give $ pure $ YieldMany (x) (xs <|> yl)
   
     



public export
Applicative m => Monad m => MonadChoice (ChoiceT m) where 
  look = MkChoiceT lookChoice
  give = giveChoice


covering
public export
Monad m => Bifunctor (\a => \b => ChoiceT m (Either a b)) where
  bimap f g = map (bimap f g)

export
MonadState s m => MonadState s (ChoiceT m) where 
  get = lift get
  put = lift . put
