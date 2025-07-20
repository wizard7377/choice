module Data.Choice.Instances 
import Data.Choice.Types
import Data.Choice.Internal
import Control.Monad.Trans
--%default total
public export
Monad f => Functor (ChoiceT f) where 
  map f (MkChoiceT c) = MkChoiceT $ mapMStep f <$> c

public export
Monad m => Applicative (ChoiceT m) where 
  pure = MkChoiceT . pure . delay . pureMStep
  a <*> b = ?h6
  --(MkChoiceT f) <*> (MkChoiceT c) = MkChoiceT $ appMStep <$> f <*> c 
  
public export
partial
Monad m => Monad (ChoiceT m) where
   (>>=) = flip bindChoiceT
   join = joinChoiceT
  
  
public export
Monad m => Functor (MStep m) where 
  map f c = mapMStep f c
public export
Monad m => Applicative (MStep m) where 
  pure = pureMStep
  f <*> c = appMStep f c
public export
Monad m => Functor (MList m) where 
  map f c = do 
    c' <- c
    let c2 = forceList c'
    ?h4
public export
Monad m => Functor (MList m) => Applicative (MList m) where 
  pure = pure . delay . pure
  f <*> c = ?h3 $ appMStep <$> (forceList <$> f) <*> (forceList <$> c)
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
  empty = MkChoiceT $ pure $ delay MNil
  a <|> b = appendChoiceT a b
  
public export
(Foldable m, Monad m) => Foldable (ChoiceT m) where
  foldr = foldrChoiceT

public export
(Traversable m, Monad m) => Traversable (ChoiceT m) where 
  traverse = traverseChoiceT
public export
[ChoiceAlt] MonadChoice m => Alternative m where 
  empty = give $ pure Nothing
  xl <|> yl = case (!(look xl)) of 
    Nothing => yl
    Just (x, xs) => give $ pure (Just (x, xs <|> yl))



-- TODO: Cleanup
private
lookChoice : (Applicative m, Monad m) => ChoiceT m a -> Yield (ChoiceT m) a
lookChoice (MkChoiceT c) = MkChoiceT $ do 
  c' <- c
  case c' of 
    MNil => pure $ delay MNil
    MCons x xs => do 
        xs' : MStep m a <- xs
        pure $ case xs' of
            MNil => MCons (Just (x, empty)) $ pure $ delay MNil
            MCons y ys => (MCons (Just (x, MkChoiceT xs)) $ pure $ delay MNil)
  
-- TODO: Cleanup
private
giveChoice : Monad m => Yield (ChoiceT m) a -> ChoiceT m a
giveChoice c = case !c of 
  Nothing => MkChoiceT $ pure $ delay MNil 
  --Just (x, xs) => (MkChoiceT $ pure (MCons x $ pure $ lateMNil)) <|> (xs)
  _ => ?h5
public export
Monad m => MonadChoice (ChoiceT m) where 
  look = lookChoice
  give = giveChoice

