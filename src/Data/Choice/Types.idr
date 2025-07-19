module Data.Choice.Types

%default total
public export 
Yield : (m : Type -> Type) -> (a : Type) -> Type 
Yield m a = m (Maybe (a, m a))
  
covering
public export 
data MStep : forall k. (k -> Type) -> k -> Type where 
    MNil : MStep m a
    MCons : a -> m (MStep m a) -> MStep m a
public export 
MList : (Type -> Type) -> Type -> Type
MList m a = m (MStep {k = Type} m a)
public export 
data ChoiceT : (m : Type -> Type) -> (a : Type) -> Type where 
    MkChoiceT : MList m a -> ChoiceT m a
  
public export
mkChoiceT : MList m a -> ChoiceT m a
mkChoiceT = MkChoiceT
public export
runChoiceT : ChoiceT m a -> MList m a
runChoiceT (MkChoiceT x) = x

public export
interface (Functor m, Applicative m, Monad m) => MonadChoice m where 
  ||| "Look" into the inner state of the monad
  look : forall a. m a -> Yield m a 
  ||| "Give" into the inner state of the monad.
  give : forall a. Yield m a -> m a
  ||| The Prolog cut, `!`, that is, if something yields some number of results make it yield one or zero
  cut : forall a. m a -> m a
  cut m = do 
    m' <- look m
    case m' of
      Nothing => give $ pure Nothing
      Just (v, _) => do 
        e <- give $ pure Nothing
        z <- give $ pure $ Just (v , e)
        pure z

  
public export 
ChoiceTArrow : (Type -> Type) -> Type -> Type -> Type
ChoiceTArrow m a b = a -> (ChoiceT m b)

  
public export 
Choice : Type -> Type
Choice = ChoiceT id

  
public export 
Context : (t : (Type -> Type) -> Type -> Type) -> (m : Type -> Type) -> (a : Type) -> Type
Context t m a = t (ChoiceT m) a
