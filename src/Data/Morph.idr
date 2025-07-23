module Data.Morph

import Control.Monad.State
import Control.Monad.Writer

hoistType : ((Type -> Type) -> Type -> Type) -> Type
hoistType t = {m : Type -> Type} -> {n : Type -> Type} -> {a : Type} -> Monad m => Monad n => ({any : Type} -> m any -> n any) -> t m a -> t n a
public export 
interface MonadFunctor (t : (Type -> Type) -> Type -> Type) where
  hoist : hoistType t

export
hoistState : {st : Type} -> hoistType (StateT st)
hoistState f v = (ST $ \s => f (v.runStateT' s))
export
hoistWriter : {st : Type} -> hoistType (WriterT st)
hoistWriter f v = (MkWriterT $ \s => f (v.unWriterT s))
export
{stateType : Type} -> MonadFunctor (StateT stateType) where 
  hoist = hoistState
export
{s : Type} -> MonadFunctor (WriterT s) where 
  hoist = hoistWriter

