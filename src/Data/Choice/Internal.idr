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
  

bindList : forall a, b, m. Monad m => (a -> MList m b) -> MList m a -> MList m b
bindList f c = do
  r0 <- mapMStep f <$> c
  let r1 = joinMList' r0
  r1

export
bindChoice : forall a, b, m. Monad m => (a -> Choice m b) -> Choice m a -> Choice m b
bindChoice f (MkChoice c) = MkChoice $ (bindList (runChoice . f) c)

  
export 
appendChoice : forall a, m. Monad m => Choice m a -> Choice m a -> Choice m a
appendChoice (MkChoice c0) (MkChoice c1) = MkChoice (appendMList c0 c1)
joinChoice' : forall a, m. Monad m => MList m (Choice m a) -> MList m a
joinChoice' c = 
  case !c of
        MNil => pure MNil
        MCons (MkChoice x) xs => 
          appendMList x (joinChoice' xs) 

 

export 
joinChoice : forall a, m. Monad m => Choice m (Choice m a) -> Choice m a
joinChoice (MkChoice c) = MkChoice (joinChoice' c)

export 
liftChoice : Monad m => m a -> Choice m a
liftChoice v = MkChoice (MCons <$> v <*> (pure $ pure MNil))

foldChoice' : (Foldable m, Monad m) => (e -> a -> a) -> a -> MList m e -> m a
foldChoice' f a c = 
  case !c of
    MNil => pure a
    MCons y ys => foldChoice' f (f y a) ys
export
foldChoice : (Monoid a, Foldable m, Monad m) => (e -> a -> a) -> a -> Choice m e -> a
foldChoice f a (MkChoice c) = let 
  r = foldChoice' f a c 
  in foldMap id r
  

export 
traverseChoice' : (Applicative f, Traversable f, Traversable m, Monad m) => (a -> f b) -> MStep m a -> f (MStep m b)
traverseChoice' f MNil = pure MNil
traverseChoice' f (MCons x xs) = ?tc
