module Control.Monad.Alternative.Choice.Internal
import Control.Monad.Alternative.Choice.Types
import Control.Monad.Alternative.Class
import Control.Monad.Trans
import Control.Monad.Morph


  
altC : ChoiceT1 m a -> ChoiceT1 m a -> AltTree m a
altC = AltBranch
%auto_lazy off
mapAltTree : Functor f => (a -> b) -> AltTree f a -> AltTree f b
mapAltTree m x = case x of 
  AltLeaf x' => AltLeaf $ delay $ m $ force x'
  AltBranch c0 c1 => altC (mapAltTree m <$> c0) (mapAltTree m <$> c1)


public export
[FunctorTree] Functor f => Functor (AltTree f) where 
  map = mapAltTree
  
public export
[FunctorChoice1] Functor f => Functor (ChoiceT1 f) where 
  map f = map (map @{FunctorTree} f)

flattenChoiceT1 : Monad m => ChoiceT1 m (ChoiceT1 m a) -> ChoiceT1 m a
flattenChoiceT1 m = case !m of 
  AltLeaf x => force x
  AltBranch c0 c1 => pure $ altC (flattenChoiceT1 c0) (flattenChoiceT1 c1)

appChoiceT1 : Monad m => ChoiceT1 m (a -> b) -> ChoiceT1 m a -> ChoiceT1 m b
appChoiceT1 f a = flattenChoiceT1 (map @{FunctorChoice1} (\g => map @{FunctorChoice1} g a) f)

pureTree : a -> AltTree m a
pureTree x = AltLeaf $ delay x

pureChoiceT1 : Monad m => a -> ChoiceT1 m a
pureChoiceT1 x = pure $ AltLeaf $ delay x

[AppChoice1] Functor (ChoiceT1 f) => Monad f => Applicative (ChoiceT1 f) using FunctorChoice1 where 
  pure = pureChoiceT1
  (<*>) = appChoiceT1

bindChoiceT1 : Monad m => ChoiceT1 m a -> (a -> ChoiceT1 m b) -> ChoiceT1 m b
bindChoiceT1 m f = flattenChoiceT1 (map @{FunctorChoice1} f m)

[MonadChoice1] Functor (ChoiceT1 m) => Applicative (ChoiceT1 m) => Monad m => Monad (ChoiceT1 m) where 
  (>>=) = assert_total $ bindChoiceT1 -- TODO: 

onlyMaybe : (a -> b) -> Maybe a -> Maybe b
onlyMaybe f = map f

foldChoice1 : Monad m => Foldable m => Monoid w => ChoiceT1 m w -> w 
foldChoice1 v = concat $ v <&> \case
  AltLeaf x => force x
  AltBranch c0 c1 => foldChoice1 c0 <+> foldChoice1 c1

[Semicat] Semigroup (a -> a) where 
  (<+>) = (.)
[Semimon] Monoid (a -> a) using Semicat where
  neutral = id
foldrChoice1 : Monad m => Foldable m => (e -> a -> a) -> a -> ChoiceT1 m e -> a
foldrChoice1 f acc c = (concat @{Semimon} (go c)) acc
  where 
    go : ChoiceT1 m e -> m (a -> a)
    go c = case !c of 
      AltLeaf x => pure $ f $ force x
      AltBranch c0 c1 => do
        f0 <- go c0
        f1 <- go c1
        pure $ f1 . f0

[FoldableChoice1] Monad m => Foldable m => Foldable (ChoiceT1 m) where
  foldr = foldrChoice1