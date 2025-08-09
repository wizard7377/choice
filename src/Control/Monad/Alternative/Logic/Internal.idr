module Control.Monad.Alternative.Logic.Internal 

import Control.Monad.Alternative.Logic.Types
mapLogicC : (a -> b) -> LogicC m a -> LogicC m b
mapLogicC f l c e = l (c . f) e

[FunctorLogicC] Functor (LogicC m) where
  map = mapLogicC

bindLogicC : LogicC m a -> (a -> LogicC m b) -> LogicC m b
bindLogicC l f = \sk, fk => l (\a, fk' => (f a) sk fk') fk

appLogicC : LogicC m (a -> b) -> LogicC m a -> LogicC m b
appLogicC f a =  \sk, fk => f (\g, fk' => a (sk . g) fk') fk

[ApplicativeLogicC] Functor (LogicC m) => Applicative (LogicC m) where
  pure x = \sk, fk => sk x fk
  (<*>) = appLogicC
[MonadLogicC] Applicative (LogicC m) => Monad (LogicC m) where
  (>>=) = bindLogicC
