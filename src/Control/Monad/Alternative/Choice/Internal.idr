module Control.Monad.Alternative.Choice.Internal
import Control.Monad.Alternative.Choice.Types
import Control.Monad.Alternative.Class
import Control.Monad.Trans
import Data.Morph
appendChoiceT1 : Monad m => TreeT m a -> ChoiceT1 m a -> ChoiceT1 m a
appendChoiceT1 c0 c1 = pure (MBranch c0 c1)

appendChoiceT1' : Monad m => ChoiceT1 m a -> ChoiceT1 m a -> ChoiceT1 m a
appendChoiceT1' c0 c1 = pure $ MBranch !c0 c1

  
pureTreeT' : Monad m => Lazy a -> TreeT m a
pureTreeT' x = MLeaf $ pure x
pureTreeT : Monad m => Lazy (List a) -> TreeT m a
pureTreeT xs = MLeaf xs
joinTreeT : Monad m => TreeT m (ChoiceT1 m a) -> ChoiceT1 m a
joinTreeT c = case c of 
  MLeaf [] => pure $ MLeaf []
  MLeaf (x :: xs) => foldr (\el, ac => appendChoiceT1' el ac) x xs
  MBranch c0 c1 => pure $ MBranch !(joinTreeT c0) (joinTreeT =<< c1)

joinChoiceT1 : Monad m => ChoiceT1 m (ChoiceT1 m a) -> ChoiceT1 m a
joinChoiceT1 m = assert_total $ joinTreeT =<< m
mapTreeT : Monad m => (a -> b) -> TreeT m a -> TreeT m b
mapTreeT f x = case x of
  MLeaf v => MLeaf $ f <$> v
  MBranch c0 c1 => MBranch (delay $ mapTreeT f c0) (mapTreeT f <$> c1)
mapChoiceT1 : Monad m => (a -> b) -> ChoiceT1 m a -> ChoiceT1 m b
mapChoiceT1 f x = case !x of
  MLeaf v => pure $ MLeaf $ f <$> v
  MBranch c0 c1 => appendChoiceT1 (mapTreeT f c0) (mapChoiceT1 f c1)

public export 
[ListFunctor] Monad m => Functor (ChoiceT1 m) where 
  map f = mapChoiceT1 f
public export 
[StepFunctor] Monad m => Functor (TreeT m) where 
  map f = mapTreeT f

appTreeT : Monad m => ChoiceT1 m (a -> b) -> ChoiceT1 m a -> ChoiceT1 m b
appTreeT f x = let r = mapChoiceT1 pro f in joinChoiceT1 r
  where 
    pro : (a -> b) -> ChoiceT1 m b
    pro g = mapChoiceT1 g x

private 
[AppList] Monad m => Functor (ChoiceT1 m) => Applicative (ChoiceT1 m) using ListFunctor where
  pure x = pure $ pureTreeT $ pure x
  f <*> x = appTreeT f x

bindListT : Monad m => ChoiceT1 m a -> (a -> ChoiceT1 m b) -> ChoiceT1 m b
bindListT x f = assert_total $ joinChoiceT1 (map @{ListFunctor} f x)
  
[MonadList] Applicative (ChoiceT1 m) => Monad m => Monad (ChoiceT1 m) using AppList where 
  (>>=) = bindListT
  join = joinChoiceT1

liftChoiceT1 : Monad m => m a -> ChoiceT1 m a
liftChoiceT1 m = (pureTreeT' . delay) <$> m
  
[TransList] MonadTrans ChoiceT1 where 
  lift = liftChoiceT1

emptyTreeT : {m : Type -> Type} -> {a : Type} -> TreeT m a 
emptyTreeT = MLeaf []

[AltList] Applicative (ChoiceT1 m) => Monad m => Alternative (ChoiceT1 m) where 
  empty = pure $ MLeaf []
  a <|> b = appendChoiceT1' a b

foldrList : {0 elem : Type} -> {0 acc : Type} -> Foldable m => Monad m => (elem -> acc -> acc) -> acc -> ChoiceT1 m elem -> acc
foldrList op init l = let 
    t0 : ChoiceT1 m (acc -> acc) = map @{ListFunctor} op l
    f0 = go =<< t0
    fn : acc -> acc = foldr ((.)) id f0
    in fn init
  where 
    go : TreeT m (acc -> acc) -> m (acc -> acc)
    go l = case l of 
      MLeaf x => pure $ foldr ((.)) id x
      MBranch a b => 
        let 
            g = (go a) 
            h = (go =<< b)
        in ((.)) <$> g <*> h
   
[FoldList] {m : Type -> Type} -> Foldable m => Monad m => Foldable (ChoiceT1 m) where
   foldr = foldrList       

        
traverseList : Traversable m => Monad m => Applicative f => (a -> f b) -> ChoiceT1 m a -> f (ChoiceT1 m b)
traverseList f l = ?_

[TraverseList] Functor (ChoiceT1 m) => Monad m => Foldable (ChoiceT1 m) => Traversable (ChoiceT1 m) using FoldList where
  traverse = ?t0
[MorphChoice] MonadFunctor ChoiceT1 where
  hoist = ?hc

