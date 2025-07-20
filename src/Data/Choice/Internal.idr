module Data.Choice.Internal
import Data.Choice.Types


mutual 
    export
    appendMList : Monad m => MList m a -> MList m a -> MList m a
    appendMList xs ys = (`appendMList'` ys) =<< xs
    export
    appendMList' : Monad m => MStep m a -> MList m a -> MList m a
    appendMList' MNil ys = ys
    appendMList' (MCons x xs) ys = pure $ MCons x (appendMList xs ys)

mutual 
    export
    joinMList : Monad m => MList m (MList m a) -> MList m a 
    joinMList = (=<<) joinMList'
    export
    joinMList' : Monad m => MStep m (MList m a) -> MList m a
    joinMList' MNil = pure MNil
    joinMList' (MCons x xs) = appendMList x (joinMList xs)

export
mapMStep : Monad m => (a -> b) -> MStep m a -> MStep m b
mapMStep _ MNil = MNil
mapMStep f (MCons x xs) = MCons (f x) (mapMStep f <$> xs)
 
export
pureMStep : Monad m => a -> MStep m a 
pureMStep v = MCons v $ pure MNil
export
appMStep : forall a, b, m. (Monad m) => MStep m (a -> b) -> MStep m a -> MStep m b
appMStep MNil _ = MNil
appMStep _ MNil = MNil
appMStep (MCons f fs) (MCons x xs) = MCons (f x) $ do
  xs' : MStep m a <- xs
  fs' : MStep m (a -> b) <- fs
  let r1 = mapMStep f xs'
  let r2 = appMStep fs' (pureMStep x)
  let r3 = appMStep fs' xs'
  let rB = appendMList' r1 $ pure r2
  let rC = appendMList rB $ pure r3
  rC
  

export
bindList : forall a, b, m. Monad m => (a -> MList m b) -> MList m a -> MList m b
bindList f c = do
  r0 <- mapMStep f <$> c
  let r1 = joinMList' r0
  r1

export
bindChoiceT : forall a, b, m. Monad m => (a -> ChoiceT m b) -> ChoiceT m a -> ChoiceT m b
bindChoiceT f (MkChoiceT c) = MkChoiceT $ (bindList (runChoiceT . f) c)

  
export 
appendChoiceT : forall a, m. Monad m => ChoiceT m a -> ChoiceT m a -> ChoiceT m a
appendChoiceT (MkChoiceT c0) (MkChoiceT c1) = MkChoiceT (appendMList c0 c1)
joinChoiceT' : forall a, m. Monad m => MList m (ChoiceT m a) -> MList m a
joinChoiceT' c = 
  case !c of
        MNil => pure MNil
        MCons (MkChoiceT x) xs => 
          appendMList x (joinChoiceT' xs) 

 

export 
joinChoiceT : forall a, m. Monad m => ChoiceT m (ChoiceT m a) -> ChoiceT m a
joinChoiceT (MkChoiceT c) = MkChoiceT (joinChoiceT' c)

export 
liftChoiceT : Monad m => m a -> ChoiceT m a
liftChoiceT v = MkChoiceT (MCons <$> v <*> (pure $ pure MNil))

foldMapChoiceT' : Monoid b => Monad m => Foldable m => (a -> b) -> MList m a -> b
foldMapChoiceT' f t = foldMap id $ do 
  t' <- t 
  (case t' of
    MNil => pure $ neutral 
    MCons y ys => pure $ f y <+> (foldMapChoiceT' f ys))
  
  
private 
[funcSemi] Semigroup (a -> a) where 
  (<+>) = (.)
private
[funcMono] Monoid (a -> a) using funcSemi where 
  neutral = id
private
helpFold : Monoid (c -> c) => Monad m => Foldable m => (a -> (c -> c)) -> MList m a -> (c -> c)
helpFold = foldMapChoiceT'
foldrChoiceT' : Monad m => Foldable m => (a -> b -> b) -> b -> MList m a -> b
foldrChoiceT' f a c = (helpFold @{funcMono} f c) a
  
public export 
foldrChoiceT : Monad m => Foldable m => (a -> b -> b) -> b -> ChoiceT m a -> b
foldrChoiceT f a (MkChoiceT c) = (foldrChoiceT' f a c)

mutual
    export 
    traverseStep : (Traversable m, Monad m, Applicative f) => (a -> f b) -> MStep m a -> f (MStep m b)
    traverseStep f (MCons x xs) =
        let
            y0 = f x
            y1 = traverseList f xs
        in
        MCons <$> y0 <*> y1
    traverseStep f MNil = pure MNil
    traverseList : (Traversable m, Monad m, Applicative f) => (a -> f b) -> MList m a -> f (MList m b)
    traverseList f = traverse (traverseStep f)

public export
traverseChoiceT : (Traversable m, Monad m, Applicative f) => (a -> f b) -> ChoiceT m a -> f (ChoiceT m b)
traverseChoiceT f (MkChoiceT m) = MkChoiceT <$> traverseList f m



