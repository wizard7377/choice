module Control.Monad.Alternative.Logic.Types 

data LogicT : {k : Type} -> (k -> Type) -> k -> Type where 
    MkLogicT : ({0 r : k} -> (cont : a -> m r -> m r) -> (end : m r) -> m r) -> LogicT m a
