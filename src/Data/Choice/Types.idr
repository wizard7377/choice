module Data.Choice.Types

%default total
public export 
Yield : (Type -> Type) -> Type -> Type 
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
data Choice : (Type -> Type) -> Type -> Type where 
    MkChoice : MList m a -> Choice m a
  
public export
mkChoice : MList m a -> Choice m a
mkChoice = MkChoice
public export
runChoice : Choice m a -> MList m a
runChoice (MkChoice x) = x

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
ChoiceArrow : (Type -> Type) -> Type -> Type -> Type
ChoiceArrow m a b = a -> (Choice m b)

  
