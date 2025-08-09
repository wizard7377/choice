module Control.Monad.Alternative.Proof.Equiv

import Control.Monad.Alternative.Proof.Common
import Control.Monad.Alternative.Logic.Types
import Control.Monad.Alternative.List.Types

projA : {m : Type -> Type} -> Monad m => {a : Type} -> (LogicT m a) -> (ListT m a)
projA {m} {a} (MkLogicT cont) = MkListT $ cont (pure {f = m} .: MCons) (pure MNull)

--projB : {m : Type -> Type} -> Monad m => {a : Type} -> (ListT m a) -> (LogicT m a)
--projB {m} {a} (MkListT step) = MkLogicT $ \cont, end => foldr cont (pure end) step
%hint
isoLogic : {m : Type -> Type} -> Monad m => {a : Type} -> WfIsomorphism (LogicT m a) (ListT m a)
isoLogic = ?_