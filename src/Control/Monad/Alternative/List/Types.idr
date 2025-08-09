module Control.Monad.Alternative.List.Types 

data StepT : (Type -> Type) -> Type -> Type where 
    MNull : StepT m a 
    MCons : a -> m (StepT m a) -> StepT m a
data ListT : (Type -> Type) -> Type -> Type where 
    MkListT : m (StepT m a) -> ListT m a