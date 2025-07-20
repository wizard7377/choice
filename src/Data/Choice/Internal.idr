module Data.Choice.Internal
import Data.Choice.Types

%auto_lazy on

mutual 
    export
    appendMList : forall m, a. Monad m => (MList m a) -> (MList m a) -> MList m a
    appendMList xs ys =  ((flip appendMList') (ys)) =<< (force <$> xs)
    export
    appendMList' : forall m, a. Monad m => (MStep m a) -> (MList m a) -> MList m a
    appendMList' MNil ys = ys
    appendMList' (MCons x xs) ys = pure $ delay $ MCons x ( (appendMList xs ys))

mutual 
    export
    joinMList : Monad m => MList m (MList m a) -> MList m a 
    joinMList c = joinMList' =<< (force <$> c)
    export
    joinMList' : Monad m => MStep m (MList m a) -> MList m a
    joinMList' MNil = pure $ delay MNil
    joinMList' (MCons x xs) = appendMList x (joinMList $ delay xs)

export
mapMStep : Monad m => (a -> b) -> Lazy (MStep m a) -> Lazy (MStep m b)
mapMStep _ MNil = MNil
mapMStep f (MCons x xs) = MCons (f x) (mapMStep f <$> xs)
 
export
pureMStep : Monad m => a -> MStep m a 
pureMStep v = MCons v $ pure $ delay MNil
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
  let rB = appendMList' r1 $ pure $ delay r2
  let rC = appendMList rB $ pure $ delay r3
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
        MNil => pure $ delay MNil
        MCons (MkChoiceT x) xs => 
          appendMList x (joinChoiceT' xs) 

 

export 
joinChoiceT : forall a, m. Monad m => ChoiceT m (ChoiceT m a) -> ChoiceT m a
joinChoiceT (MkChoiceT c) = MkChoiceT (joinChoiceT' c)

export 
liftChoiceT : forall m, a. Monad m => m a -> ChoiceT m a
liftChoiceT v = MkChoiceT $ do
  let r0 : (Lazy (MStep m a)) = delay MNil 
  v' <- v
  pure $ delay $ MCons v' (pure r0)

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

twofmap : Functor f => Functor g => (a -> b) -> f (g a) -> (f (g b))
twofmap h a = (map (map h) a)
mutual
    export 
    traverseStep : (Traversable m, Monad m, Applicative t) => (a -> t b) -> MStep m a -> t (MStep m b)
    traverseStep f (MCons x xs) =
        let
            y0 = f x
            y1 = traverseList f xs
        in
        MCons <$> ( y0) <*> y1
    traverseStep f MNil = pure MNil
    traverseList : (Traversable m, Monad m, Applicative t) => (a -> t b) -> MList m a -> t (MList m b)
    traverseList f c = sequence $ do 
      c' <- c
      let x = traverseStep f c'
      let y = delay <$> x
      pure y
      --delay <$> x

public export
traverseChoiceT : (Traversable m, Monad m, Applicative f) => (a -> f b) -> ChoiceT m a -> f (ChoiceT m b)
traverseChoiceT f (MkChoiceT m) = MkChoiceT <$> traverseList f m



