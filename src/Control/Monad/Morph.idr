module Control.Monad.Morph

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.RWS

public export
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
export 
hoistReader : {r : Type} -> hoistType (ReaderT r)
hoistReader f v = (MkReaderT $ \r => f (v.runReaderT' r))

{r : Type} -> MonadFunctor (ReaderT r) where 
  hoist = hoistReader
hoistRWS : {r : Type} -> {w : Type} -> {s : Type} -> Monoid w => hoistType (RWST r w s)
hoistRWS f v = (rwsT $ \r, s => f (runRWST r s v))
{r : Type} -> {w : Type} -> {s : Type} -> Monoid w => MonadFunctor (RWST r w s) where 
  hoist = hoistRWS