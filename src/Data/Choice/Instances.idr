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
      Just v => pure $ Just $ mapMStep f v

public export
Monad m => Applicative (ChoiceT m) where 
  pure = MkChoiceT . pure . Just . pureMStep
  (MkChoiceT f) <*> (MkChoiceT c) = MkChoiceT $ do 
    f' <- f 
    c' <- c 
    case f' of 
      Nothing => pure Nothing
      Just f'' => case c' of 
        Nothing => pure Nothing
        Just c'' => pure $ Just $ appMStep f'' c'' 
  
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
  map f c =  
    case !c of 
      Nothing => pure $ Nothing
      Just v => pure $ Just $ f <$> v
public export
Monad m => Functor (MList m) => Applicative (MList m) where 
  pure = pure . Just . pure
  (<*>) = appMList
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
(Foldable m, Monad m, Traversable m) => Foldable (ChoiceT m) where
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


public export 
[unfill] Monad m => Cast (MList1 m a) (MList m a) where
  cast l = Just <$> l
  
public export 
[listChoice] Cast (MList m a) (ChoiceT m a) where 
  cast l = MkChoiceT l
public export 
[choiceList] Cast (ChoiceT m a) (MList m a) where 
  cast (MkChoiceT v) = v
  
public export 
Monad m => Cast (MList1 m a) (ChoiceT m a) where 
  cast l = cast @{listChoice} $ cast @{unfill} l
private 
getFirstAndRest : Monad m => MList1 m a -> m (Lazy a , Maybe (MList1 m a))
getFirstAndRest c = case !c of 
  MOne x => pure (delay x, Nothing)
  MApp x y => do 
    cs : (a , Maybe _ ) <- getFirstAndRest $ pure $ x
    case cs of 
      (v, Nothing) => pure (delay v, Just y)
      (v, Just r) => pure (delay v, Just $ appendMList r y)
    
  

-- TODO: Cleanup
private
lookChoice : forall a, m. (Applicative m, Monad m) => ChoiceT m a -> Yield (ChoiceT m) a
lookChoice (MkChoiceT c) = MkChoiceT $ do 
  Just c' <- c | Nothing => pure Nothing
  case c' of 
    MOne x => pure $ Just $ MOne $ Just $ (delay x, empty)
    MApp x y => do 
      (v, r) : (a , Maybe (MList1 m a) ) <- getFirstAndRest $ pure $ force x 
      (v2 , r2) : (a , MList1 m a) <- case r of 
            Just r' => let tem : m (a , m (MStep m a)) = (MkPair v) <$> (pure $ appendMList r' y) in tem
            Nothing => let tem2 : m (a , m (MStep m a)) = (MkPair v) <$> (pure $ y) in tem2
      pure $ Just $ MOne $ Just $ (delay v2, cast r2)

 
-- TODO: Cleanup
private
giveChoice : Monad m => Yield (ChoiceT m) a -> ChoiceT m a
giveChoice c = do 
  Just (v,r) <- c | Nothing => MkChoiceT $ pure $ Nothing
  MkChoiceT $ pure $ Just $ MApp (MOne v) $ ?gcr
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


