module Control.Monad.Alternative.List.Types 

public export
data StepT : (Type -> Type) -> Type -> Type where 
    MNull : StepT m a 
    MCons : a -> m (StepT m a) -> StepT m a
public export
data ListT : (Type -> Type) -> Type -> Type where 
    MkListT : m (StepT m a) -> ListT m a

public export 
mkListT : Monad m => m (StepT m a) -> ListT m a
mkListT = MkListT

public export
unListT : ListT m a -> m (StepT m a)
unListT (MkListT m) = m