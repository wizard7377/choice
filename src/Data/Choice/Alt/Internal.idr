module Data.Choice.Alt.Internal
import Data.Choice.Alt.Types
import Data.Choice.Class
export
appendListT' : Monad m => StepT m a -> ListT1 m a -> StepT m a
appendListT' v = MApp (force v)
%inline
export
appendListT : Monad m => ListT1 m a -> ListT1 m a -> ListT1 m a
appendListT xs ys = (pure . (`MApp` ys)) =<< xs
    
        
       

mutual 
    export
    joinListT : Monad m => ListT1 m (ListT m a) -> ListT m a 
    joinListT = (=<<) joinListT'
    export
    joinListT' : Monad m => StepT m (ListT m a) -> ListT m a
    joinListT' (MOne y) = y
    joinListT' (MApp x xs) = do
      y <- joinListT' x 
      ys <- joinListT xs 
      case y of 
        Nothing => pure ys
        Just y' => case ys of 
          Nothing => pure $ Just $ y'
          Just ys' => pure $ Just (MApp y' $ pure ys')

export
mapStepT : Monad m => (a -> b) -> StepT m a -> StepT m b
mapStepT f (MOne x) = MOne (f x)
mapStepT f (MApp x xs) = MApp (mapStepT f x) (mapStepT f <$> xs)
 
export 
comapStepT : Monad m => StepT m (a -> b) -> a -> StepT m b
comapStepT (MOne f) y = MOne (f y)
comapStepT (MApp f fs) y = MApp (comapStepT f y) ((flip comapStepT) y <$> fs)
remapStep : Monad m => a -> StepT m (a -> b) -> StepT m b
remapStep = flip comapStepT
private 
Monad m => Semigroup (ListT1 m a) where 
  (<+>) = appendListT
export
pureStepT : Monad m => a -> StepT m a 
pureStepT v = MOne v
export
appStepT : forall a, b, m. (Monad m) => StepT m (a -> b) -> StepT m a -> StepT m b
appStepT (MOne f) (MOne x) = MOne (f x)
appStepT (MApp f fs) (MOne x) = MApp (comapStepT f x) (remapStep x <$> fs)
appStepT (MOne f) (MApp x xs) = MApp (mapStepT f x) (mapStepT f <$> xs)
appStepT (MApp f fs) (MApp x xs) = MApp (appStepT f x) $ do
  xs' <- xs
  fs' <- fs
  let r1 = appStepT f xs'
  let r2 = appStepT fs' x
  let r3 = appStepT fs' xs'
  let rB = MApp r1 $ pure r2
  let rC = appendListT (pure rB) $ pure r3
  rC
  
public export
mapListT : Monad m => (a -> b) -> ListT m a -> ListT m b
mapListT f x = do
  Just x' <- x | Nothing => pure Nothing
  pure $ Just $ mapStepT f x'
 
public export
appListT : Monad m => ListT m (a -> b) -> ListT m a -> ListT m b
appListT f x = do 
  Just f' <- f | Nothing => pure Nothing 
  Just x' <- x | Nothing => pure Nothing
  pure $ Just $ appStepT f' x'
 
{-
export
bindList : forall a, b, m. Monad m => (a -> ListT1 m b) -> ListT1 m a -> ListT1 m b
bindList f c = do
  r0 <- mapStepT f <$> c
  let r1 = ?h10 -- joinListT' r0
  r1
-}
bindList' : forall a, b, m. Monad m => (a -> ListT m b) -> ListT m a -> ListT m b
bindList' f c = case !c of 
  Nothing => pure Nothing 
  Just (MOne x) => f x
  Just (MApp x y) => do
    rx <- joinListT' $ mapStepT f x
    ry <- joinListT' $ mapStepT f !y 
    case rx of 
      Nothing => pure $ ry 
      Just rx' => case ry of 
        Nothing => pure $ rx
        Just ry' => pure $ Just $ MApp rx' (pure ry')
    
  
export
bindChoiceT : forall a, b, m. Monad m => (a -> ChoiceT m b) -> ChoiceT m a -> ChoiceT m b
bindChoiceT f (MkChoiceT c) = MkChoiceT $ ((bindList' (runChoiceT . f)) c)

  
export 
appendChoiceT : forall a, m. Monad m => ChoiceT m a -> ChoiceT m a -> ChoiceT m a
appendChoiceT (MkChoiceT c0) (MkChoiceT c1) = MkChoiceT $ do
  c0' <- c0
  c1' <- c1
  case c0' of 
    Nothing => c1
    Just x => case c1' of
      Nothing => c0 
      Just y => pure $ Just $ (MApp x $ pure y) 
  
joinChoiceT'' : forall a, m. Monad m => StepT m (ChoiceT m a) -> ListT m a
joinChoiceT'' (MOne (MkChoiceT x)) = x
joinChoiceT'' (MApp x y) = do
  x' <- joinChoiceT'' x
  y' <- joinChoiceT'' <$> y
  case x' of 
    Nothing => y'
    Just x'' => case !y' of 
      Nothing => pure x'
      Just y'' => pure $ Just $ MApp x'' (pure y'')
joinChoiceT' : forall a, m. Monad m => ListT m (ChoiceT m a) -> ListT m a
joinChoiceT' c = 
  case !c of
        Nothing => pure Nothing
        Just x => joinChoiceT'' x

 

export 
joinChoiceT : forall a, m. Monad m => ChoiceT m (ChoiceT m a) -> ChoiceT m a
joinChoiceT (MkChoiceT c) = MkChoiceT (joinChoiceT' c)

export 
liftChoiceT : Monad m => m a -> ChoiceT m a
liftChoiceT v = MkChoiceT $ do
  v' <- v 
  pure $ Just $ MOne $ v'

foldMapStep : Monoid b => Monad m => Foldable m => (a -> b) -> StepT m a -> b
foldMapStep f t = case t of 
  MOne x => f x
  MApp x xs => (foldMapStep f x) <+> concat (foldMapStep f <$> xs)
foldMapChoiceT' : Monoid b => Monad m => Foldable m => (a -> b) -> ListT1 m a -> b
foldMapChoiceT' f t = foldMap id $ do 
  t' <- t 
  (case t' of
    (MOne x) => pure $ f x
    -- TODO: Cleanup
    (MApp y ys) => pure $ foldMapStep f (MApp y ys))
  
  
private 
[funcSemi] Semigroup (a -> a) where 
  (<+>) = (.)
private
[funcMono] Monoid (a -> a) using funcSemi where 
  neutral = id
private
helpFold : Monoid (c -> c) => Monad m => Foldable m => (a -> (c -> c)) -> ListT1 m a -> (c -> c)
helpFold = foldMapChoiceT'
foldrChoiceT' : Monad m => Foldable m => (a -> b -> b) -> b -> ListT1 m a -> b
foldrChoiceT' f a c = (helpFold @{funcMono} f c) a



public export 
foldrStepT : forall a, b, m. Monad m => Foldable m => (a -> b -> b) -> b -> StepT m a -> b
foldrStepT f a (MOne c) = f c a
foldrStepT f a (MApp x y) = foldrChoiceT' f a (pure $ MApp x y)
public export 
foldrListT : forall a, b, m. Monad m => Foldable m => (a -> b -> b) -> b -> ListT m a -> b
foldrListT f a c = let 
  r0 = (map $ map (foldrStepT f a)) c -- foldrChoiceT' f a c
  r1 = concat r0
  in case r1 of 
    Just y => y 
    Nothing => a
public export 
foldrChoiceT : forall a, b, m. Monad m => Foldable m => (a -> b -> b) -> b -> ChoiceT m a -> b
foldrChoiceT f a (MkChoiceT c) = let 
  r' = foldrListT f a c
  in r'
  {-
foldrChoiceT f a (MkChoiceT c) = let 
  s : (Maybe (m (StepT m _))) = sequence c
  in case s of 
    Just c' => foldrChoiceT' f a c'
    Nothing => a
-}


mutual
    export 
    traverseStep : (Traversable m, Monad m, Applicative f) => (a -> f b) -> StepT m a -> f (StepT m b)
    traverseStep f (MApp x xs) =
        let
            y0 = traverseStep f x
            y1 = traverseList f xs
        in
          MApp <$> (delay <$> y0) <*> y1
    traverseStep f (MOne x) = MOne <$> f x
    traverseList : (Traversable m, Monad m, Applicative f) => (a -> f b) -> ListT1 m a -> f (ListT1 m b)
    traverseList f = traverse (traverseStep f)
    traverseList' : (Traversable m, Monad m, Functor f, Applicative f) => (a -> f b) -> ListT m a -> f (ListT m b)
    traverseList' f c = let 
      c' : Maybe (ListT1 m a) = sequence c
      in case c' of 
        Just c'' => let 
          r = traverseList f c''
          r' = (map $ map Just) r
          in r'
        Nothing => pure $ pure $ Nothing
public export
traverseChoiceT : (Traversable m, Monad m, Applicative f) => (a -> f b) -> ChoiceT m a -> f (ChoiceT m b)
traverseChoiceT f (MkChoiceT m) = 
  MkChoiceT <$> traverseList' f m



public export 
getFirstAndRest : Monad m => ListT1 m a -> m (Lazy a , Maybe (ListT1 m a))
getFirstAndRest c = case !c of 
  MOne x => pure (delay x, Nothing)
  MApp x y => do 
    cs : (Lazy a , Maybe _ ) <- getFirstAndRest $ pure $ x
    case cs of 
      (v, Nothing) => pure (v, Just y)
      (v, Just r) => pure (v, Just $ appendListT r y)
    
  

lookChoice' : forall m, a. Applicative m => Monad m => (ListT m a) -> m (Yield (ListT m) a)
lookChoice' m = case !m of 
  Nothing => pure $ YieldZero
  Just (MOne x) => pure $ YieldOne x
  Just (MApp x y) => pure $ YieldMany (pure $ Just x) (Just <$> y)
  
  
giveChoice' : forall m, a. Applicative m => Monad m => m (Yield (ListT m) a) -> (ListT m a)
giveChoice' m = case !m of 
  YieldZero => pure Nothing
  YieldOne x => pure $ Just $ MOne x
  YieldMany x y => case !y of 
    Nothing => x
    Just y' => case !x of 
      Nothing => y 
      Just x' => pure $ Just $ MApp x' $ pure y'

raiseChoice : ListT m a -> ChoiceT m a
raiseChoice l = MkChoiceT l
lowerChoice : ChoiceT m a -> ListT m a
lowerChoice (MkChoiceT m) = m
transformYield : m (Yield (ListT m) a) -> ChoiceT m a

cotransformYield : m (Yield (ChoiceT m) a) -> ListT m a
export 
lookChoice : forall m, a. Functor m => Applicative m => Monad m => (ChoiceT m a) -> m (Yield (ListT m) a)
lookChoice x = ?h0
export
giveChoice : forall m, a. Applicative m => Monad m => m (Yield (ChoiceT m) a) -> (ListT m a)
giveChoice x = ?h1
