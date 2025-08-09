module Control.Monad.Alternative.Choice.Types
import public Data.Simple
-- import public Control.Monad.Alternative.Class
%default total

--Yield : (m : Type -> Type) -> (a : Type) -> Type 
--Yield m a = m (Maybe (Lazy a, m a))
  
||| The type of a "step" in a monadic nondeterministic computation
||| @ m The inner monad (somewhat confusingly, this is the *global* state)
||| @ a The result
covering
public export 
data TreeT : forall k0, k1. (m : k0 -> k1) -> (a : k0) -> Type where 
    ||| Yield exactly one result
    ||| @r The result
    MLeaf : (r : Lazy (List a)) -> TreeT m a
    ||| Yield two sets of results.
    ||| @c0 the first set
    ||| @c1 the second set, should be behind a monad
    MBranch : (c0 : (TreeT m a)) -> (c1 : m (TreeT m a)) -> TreeT m a
 

||| Monadically returns a `Maybe` value.
||| If there should not be any results, the result is @pure Nothing@
||| If there should be a result, return a `TreeT` containing that set
covering public export 
ChoiceT0 : forall k. (m : Type -> k) -> (a : Type) -> k 
ChoiceT0 m a = m (Maybe (TreeT {k0 = Type} m a))

||| A `ChoiceT0` that must yield at least one result, ie, without an inner `Maybe`
covering public export 
ChoiceT1 : forall k. (Type -> k) -> Type -> k 
ChoiceT1 m a = m (TreeT {k0 = Type} m a)


||| The core List Monad Transformer
||| Fundementally, this behaves as a wrapper around a list with monadic actions at each cons application
||| Thereby, the inner monad is the global state.
||| Somewhat confusingly, to get local state, we must do the outer monad
||| @m the inner monad 
||| @a the result
public export 
data ChoiceT : forall k. (m : Type -> k) -> (a : Type) -> Type where 
    MkChoiceT : ChoiceT0 m a -> ChoiceT m a

||| A smart constructor for `ChoiceT`  
public export
mkChoiceT : ChoiceT0 m a -> ChoiceT m a
mkChoiceT = MkChoiceT

||| Run a `ChoiceT` monad transformer, giving the inner value
public export
runChoiceT : ChoiceT m a -> ChoiceT0 m a
runChoiceT (MkChoiceT x) = x

||| Consume a `ChoiceT` monad transformer, yielding all results in the inner monad
public export covering
%spec m
consumeChoiceT : Monad m => ChoiceT m a -> m (List a)
consumeChoiceT (MkChoiceT x) = case !x of
    Nothing => pure []
    Just (MLeaf v) => pure v
    Just (MBranch v xs) => do
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
[unfill] Monad m => Cast (ChoiceT1 m a) (ChoiceT0 m a) where
  cast l = Just <$> l
  
public export 
[listChoice] Cast (ChoiceT0 m a) (ChoiceT m a) where 
  cast l = MkChoiceT l
public export 
[choiceList] Cast (ChoiceT m a) (ChoiceT0 m a) where 
  cast (MkChoiceT v) = v
  
public export covering
[unfillChoice] Monad m => Cast (ChoiceT1 m a) (ChoiceT m a) where 
  cast l = cast @{listChoice} $ cast @{unfill} l
