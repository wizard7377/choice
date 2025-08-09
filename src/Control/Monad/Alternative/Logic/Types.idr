module Control.Monad.Alternative.Logic.Types 

public export
0 LogicC : {k : Type} -> (m : k -> Type) -> (a : Type) -> Type
LogicC {k} m a = {0 r : k} -> (cont : a -> m r -> m r) -> (end : m r) -> m r

public export
data LogicT : {k : Type} -> (m : k -> Type) -> (a : Type) -> Type where 
    MkLogicT : LogicC m a -> LogicT m a

public export 
mkLogicT : LogicC m a -> LogicT m a
mkLogicT cont = MkLogicT cont

public export 
unLogicT : LogicT m a -> LogicC m a
unLogicT (MkLogicT cont) = cont