module Data.Choice.Class

public export
interface (Functor m, Applicative m, Monad m) => MonadChoice m where 
  ||| "Look" into the inner state of the monad
  ||| That is, provide a way to query the inner state for either a new state and a value or nothing
  look : forall a. m a -> m (Maybe (Lazy a, m a))
  ||| "Give" into the inner state of the monad.
  give : forall a. m (Maybe (Lazy a, m a)) -> m a
  ||| The Prolog cut, `!`, that is, if something yields some number of results make it yield one or zero
  cut : forall a. m a -> m a
  cut m = do 
    m' <- look m
    case m' of
      Nothing => give $ pure Nothing
      Just (v, _) => do 
        e <- give $ pure Nothing
        z <- give $ pure $ Just (v , e)
        pure z