module Data.Choice.Internal
import Data.Choice.Types

mutual 
    export
    appendMList : Monad m => MList1 m a -> MList1 m a -> MList1 m a
    appendMList xs ys = (pure . (`appendMList'` ys)) =<< xs
    export
    appendMList' : Monad m => MStep m a -> MList1 m a -> MStep m a
    appendMList' v = MApp (force v)
        
       

mutual 
    export
    joinMList : Monad m => MList1 m (MList m a) -> MList m a 
    joinMList = (=<<) joinMList'
    export
    joinMList' : Monad m => MStep m (MList m a) -> MList m a
    joinMList' (MOne y) = y
    joinMList' (MApp x xs) = do
      y <- joinMList' x 
      ys <- joinMList xs 
      case y of 
        Nothing => pure ys
        Just y' => case ys of 
          Nothing => pure $ Just $ y'
          Just ys' => pure $ Just (MApp y' $ pure ys')

export
mapMStep : Monad m => (a -> b) -> MStep m a -> MStep m b
mapMStep f (MOne x) = MOne (f x)
mapMStep f (MApp x xs) = MApp (mapMStep f x) (mapMStep f <$> xs)
 
export 
comapMStep : Monad m => MStep m (a -> b) -> a -> MStep m b
comapMStep (MOne f) y = MOne (f y)
comapMStep (MApp f fs) y = MApp (comapMStep f y) ((flip comapMStep) y <$> fs)
remapStep : Monad m => a -> MStep m (a -> b) -> MStep m b
remapStep = flip comapMStep
private 
Monad m => Semigroup (MList1 m a) where 
  (<+>) = appendMList
export
pureMStep : Monad m => a -> MStep m a 
pureMStep v = MOne v
export
appMStep : forall a, b, m. (Monad m) => MStep m (a -> b) -> MStep m a -> MStep m b
appMStep (MOne f) (MOne x) = MOne (f x)
appMStep (MApp f fs) (MOne x) = MApp (comapMStep f x) (remapStep x <$> fs)
appMStep (MOne f) (MApp x xs) = MApp (mapMStep f x) (mapMStep f <$> xs)
appMStep (MApp f fs) (MApp x xs) = MApp (appMStep f x) $ do
  xs' <- xs
  fs' <- fs
  let r1 = appMStep f xs'
  let r2 = appMStep fs' x
  let r3 = appMStep fs' xs'
  let rB = appendMList' r1 $ pure r2
  let rC = appendMList (pure rB) $ pure r3
  rC
  
public export
mapMList : Monad m => (a -> b) -> MList m a -> MList m b
mapMList f x = do
  Just x' <- x | Nothing => pure Nothing
  pure $ Just $ mapMStep f x'
 
public export
appMList : Monad m => MList m (a -> b) -> MList m a -> MList m b
appMList f x = do 
  Just f' <- f | Nothing => pure Nothing 
  Just x' <- x | Nothing => pure Nothing
  pure $ Just $ appMStep f' x'
 
{-
export
bindList : forall a, b, m. Monad m => (a -> MList1 m b) -> MList1 m a -> MList1 m b
bindList f c = do
  r0 <- mapMStep f <$> c
  let r1 = ?h10 -- joinMList' r0
  r1
-}
bindList' : forall a, b, m. Monad m => (a -> MList m b) -> MList m a -> MList m b
bindList' f c = case !c of 
  Nothing => pure Nothing 
  Just (MOne x) => f x
  Just (MApp x y) => do
    rx <- joinMList' $ mapMStep f x
    ry <- joinMList' $ mapMStep f !y 
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
  
joinChoiceT'' : forall a, m. Monad m => MStep m (ChoiceT m a) -> MList m a
joinChoiceT'' (MOne (MkChoiceT x)) = x
joinChoiceT'' (MApp x y) = do
  x' <- joinChoiceT'' x
  y' <- joinChoiceT'' <$> y
  case x' of 
    Nothing => y'
    Just x'' => case !y' of 
      Nothing => pure x'
      Just y'' => pure $ Just $ MApp x'' (pure y'')
joinChoiceT' : forall a, m. Monad m => MList m (ChoiceT m a) -> MList m a
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

foldMapStep : Monoid b => Monad m => Foldable m => (a -> b) -> MStep m a -> b
foldMapStep f t = case t of 
  MOne x => f x
  MApp x xs => (foldMapStep f x) <+> concat (foldMapStep f <$> xs)
foldMapChoiceT' : Monoid b => Monad m => Foldable m => (a -> b) -> MList1 m a -> b
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
helpFold : Monoid (c -> c) => Monad m => Foldable m => (a -> (c -> c)) -> MList1 m a -> (c -> c)
helpFold = foldMapChoiceT'
foldrChoiceT' : Monad m => Foldable m => (a -> b -> b) -> b -> MList1 m a -> b
foldrChoiceT' f a c = (helpFold @{funcMono} f c) a


public export 
foldrChoiceT : forall a, b, m. Monad m => Traversable m => Foldable m => (a -> b -> b) -> b -> ChoiceT m a -> b
foldrChoiceT f a (MkChoiceT c) = let 
  s : (Maybe (m (MStep m _))) = sequence c
  in case s of 
    Just c' => foldrChoiceT' f a c'
    Nothing => a

mutual
    export 
    traverseStep : (Traversable m, Monad m, Applicative f) => (a -> f b) -> MStep m a -> f (MStep m b)
    traverseStep f (MApp x xs) =
        let
            y0 = traverseStep f x
            y1 = traverseList f xs
        in
          MApp <$> (delay <$> y0) <*> y1
    traverseStep f (MOne x) = MOne <$> f x
    traverseList : (Traversable m, Monad m, Applicative f) => (a -> f b) -> MList1 m a -> f (MList1 m b)
    traverseList f = traverse (traverseStep f)
    traverseList' : (Traversable m, Monad m, Functor f, Applicative f) => (a -> f b) -> MList m a -> f (MList m b)
    traverseList' f c = let 
      c' : Maybe (MList1 m a) = sequence c
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



