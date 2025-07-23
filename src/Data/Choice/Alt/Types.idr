module Data.Choice.Alt.Types
import public Data.Simple
-- import public Data.Choice.Class
%default total

--Yield : (m : Type -> Type) -> (a : Type) -> Type 
--Yield m a = m (Maybe (Lazy a, m a))
  
||| The type of a "step" in a monadic nondeterministic computation
||| @ m The inner monad (somewhat confusingly, this is the *global* state)
||| @ a The result
covering
public export 
data StepT : forall k0, k1. (m : k0 -> k1) -> (a : k0) -> Type where 
    ||| Yield exactly one result
    ||| @r The result
    MOne : (r : a) -> StepT m a
    ||| Yield two sets of results.
    ||| @c0 the first set, should be lazy 
    ||| @c1 the second set, should be behind a monad
    MApp : (c0 : Lazy (StepT m a)) -> (c1 : m (StepT m a)) -> StepT m a
 
||| Monadically returns a `Maybe` value.
||| If there should not be any results, the result is @pure Nothing@
||| If there should be a result, return a `StepT` containing that set
public export 
ListT : forall k. (m : Type -> k) -> (a : Type) -> k 
ListT m a = m (Maybe (StepT {k0 = Type} m a))

||| A `ListT` that must yield at least one result, ie, without an inner `Maybe`
public export 
ListT1 : forall k. (Type -> k) -> Type -> k 
ListT1 m a = m (StepT {k0 = Type} m a)

||| The core List Monad Transformer
||| Fundementally, this behaves as a wrapper around a list with monadic actions at each cons application
||| Thereby, the inner monad is the global state.
||| Somewhat confusingly, to get local state, we must do the outer monad
||| @m the inner monad 
||| @a the result
public export 
data ChoiceT : forall k. (m : Type -> k) -> (a : Type) -> Type where 
    MkChoiceT : ListT m a -> ChoiceT m a

||| A smart constructor for `ChoiceT`  
public export
mkChoiceT : ListT m a -> ChoiceT m a
mkChoiceT = MkChoiceT

||| Run a `ChoiceT` monad transformer, giving the inner value
public export
runChoiceT : ChoiceT m a -> ListT m a
runChoiceT (MkChoiceT x) = x

||| Consume a `ChoiceT` monad transformer, yielding all results in the inner monad
public export covering
%spec m
consumeChoiceT : Monad m => ChoiceT m a -> m (List a)
consumeChoiceT (MkChoiceT x) = case !x of
    Nothing => pure []
    Just (MOne v) => pure [v]
    Just (MApp v xs) => do
      start <- consumeChoiceT $ MkChoiceT $ pure $ Just v
      rest <- consumeChoiceT $ MkChoiceT $ Just <$> xs 
      pure (start ++ rest)  

  
||| The arrow of choices, that is, `a +> b`
public export 
ChoiceArrow : {m : Type -> Type} -> Type -> Type -> Type
ChoiceArrow a b = a -> (ChoiceT m b)
 
public export 
Choice : Type -> Type
Choice = ChoiceT Simple


||| `ChoiceT` equipped with a local state 
||| @t the type of the local state
||| @m the type of the global state
||| @a the type of the result  
public export 
Line : forall k. (g : Type -> Type) -> (l : k -> Type) -> (a : k) -> Type 
Line g l a = ChoiceT g (l a)
public export covering
[unfill] Monad m => Cast (ListT1 m a) (ListT m a) where
  cast l = Just <$> l
  
public export 
[listChoice] Cast (ListT m a) (ChoiceT m a) where 
  cast l = MkChoiceT l
public export 
[choiceList] Cast (ChoiceT m a) (ListT m a) where 
  cast (MkChoiceT v) = v
  
public export covering
[unfillChoice] Monad m => Cast (ListT1 m a) (ChoiceT m a) where 
  cast l = cast @{listChoice} $ cast @{unfill} l
