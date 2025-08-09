module Control.Monad.Alternative.Choice.Types
-- import public Control.Monad.Alternative.Class
%default total

 
||| The type of a "step" in a monadic nondeterministic computation
||| @ m The inner monad (somewhat confusingly, this is the *global* state)
||| @ a The result
covering
public export 
data AltTree : (m : Type -> Type) -> (a : Type) -> Type where
  AltLeaf : Lazy a -> AltTree m a
  AltBranch : m (AltTree m a) -> m (AltTree m a) -> AltTree m a
 


covering 
public export 
ChoiceT1 : (Type -> Type) -> Type -> Type
ChoiceT1 m a = m (AltTree m a)

covering 
public export 
ChoiceT0 : (Type -> Type) -> Type -> Type
ChoiceT0 m a = m (Maybe (AltTree m a))
 

covering 
public export 
data ChoiceT : (m : Type -> Type) -> (a : Type) -> Type where 
  MkChoiceT : m (Maybe (AltTree m a)) -> ChoiceT m a

covering 
public export
mkChoiceT : m (Maybe (AltTree m a)) -> ChoiceT m a
mkChoiceT = MkChoiceT
public export
unChoiceT : ChoiceT m a -> m (Maybe (AltTree m a))
unChoiceT (MkChoiceT x) = x