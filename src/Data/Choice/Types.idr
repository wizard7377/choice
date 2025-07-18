module Data.Choice.Types

public export 
Yield : (Type -> Type) -> Type -> Type 
Yield m a = m (Maybe (a, m a))
  
public export 
data MStep : forall k. (k -> Type) -> k -> Type where 
    MNil : MStep m a
    MCons : a -> m (MStep m a) -> MStep m a
public export 
MList : (Type -> Type) -> Type -> Type
MList m a = m (MStep {k = Type} m a)
public export 
data Choice : (Type -> Type) -> Type -> Type where 
    MkChoice : MList m a -> Choice m a
  
public export
mkChoice : MList m a -> Choice m a
mkChoice = MkChoice
public export
runChoice : Choice m a -> MList m a
runChoice (MkChoice x) = x

interface MonadChoice m where 
  clook : forall a. m a -> Yield m a 
  crefl : forall a. Yield m a -> m a
