module Data.Choice.Assay.Cons.Types
import public Data.Simple

%default total
public export 
Yield : (m : Type -> Type) -> (a : Type) -> Type 
Yield m a = m (Maybe (Lazy a, m a))
  
covering
public export 
data MStep : forall k. (k -> Type) -> k -> Type where 
    MNil : MStep m a
    MCons : a -> m (MStep m a) -> MStep m a
public export 
MList : (Type -> Type) -> Type -> Type
MList m a = m (MStep {k = Type} m a)

||| The core List Monad Transformer
||| Fundementally, this behaves as a wrapper around a list with monadic actions at each cons application
||| Thereby, the inner monad is the global state.
||| Somewhat confusingly, to get local state, we must do 
||| @m the inner monad 
||| @a the result
public export 
data ChoiceT : (m : Type -> Type) -> (a : Type) -> Type where 
    MkChoiceT : MList m a -> ChoiceT m a
  
public export
mkChoiceT : MList m a -> ChoiceT m a
mkChoiceT = MkChoiceT
public export
runChoiceT : ChoiceT m a -> MList m a
runChoiceT (MkChoiceT x) = x

public export covering
consumeChoiceT : Monad m => ChoiceT m a -> m (List a)
consumeChoiceT (MkChoiceT x) = case !x of
    MNil => pure []
    MCons v xs => do
      rest <- consumeChoiceT $ MkChoiceT xs
      pure (v :: rest)  


  
public export 
ChoiceTArrow : (Type -> Type) -> Type -> Type -> Type
ChoiceTArrow m a b = a -> (ChoiceT m b)

  
public export 
Choice : Type -> Type
Choice = ChoiceT Simple

  
public export 
Context : (t : (Type -> Type) -> Type -> Type) -> (m : Type -> Type) -> (a : Type) -> Type
Context t m a = t (ChoiceT m) a
