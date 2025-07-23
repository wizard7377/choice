module Data.Choice.Class

import Data.Morph
import Control.Monad.Trans
public export 
data Yield : (m : k -> Type) -> (a : k) -> Type where 
  YieldZero : Yield m a
  YieldOne : Lazy a -> Yield m a
  YieldMany : forall b. m b -> m b -> Yield m b
  
export
implementation Functor m => Functor (Yield m) where 
  map _ YieldZero = YieldZero
  map f (YieldOne x) = YieldOne $ f x
  map f (YieldMany x y) = YieldMany (f <$> x) (f <$> y)
public export
interface (Functor m, Applicative m, Monad m) => MonadChoice m where 
  ||| "Look" into the inner state of the monad
  ||| That is, provide a way to query the inner state for either a new state and a value or nothing
  look : forall a. m a -> m (Yield m a)
  ||| "Give" into the inner state of the monad.
  give : forall a. m (Yield m a) -> m a
  ||| The Prolog cut, `!`, that is, if something yields some number of results make it yield one or zero
  cut : forall a. m a -> m a
  cut m = do 
    m' <- look m
    case m' of 
      YieldMany x y => cut x
      m'' => give $ pure m''


(MonadFunctor t, MonadChoice m, MonadTrans t, Monad (t m)) => MonadChoice (t m) where
  give = ?h0
  look = ?h1