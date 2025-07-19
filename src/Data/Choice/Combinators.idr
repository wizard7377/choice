module Data.Choice.Combinators 

import Data.Choice.Types
import Data.Choice.Instances
  
  
namespace C 

  public export 
  ||| ⊥
  fault : MonadChoice m => m a
  fault = give $ pure Nothing

  public export
  ||| Get a value, and then run another computation
  ||| Equivalent to monadic `>>=`
  andThen : MonadChoice m => m a -> (a -> m b) -> m b
  andThen = (>>=)
  
  public export
  ||| Conjunction of two statements, equivalent to `(,)` over the monad
  and : MonadChoice m => m a -> m b -> m (a, b)
  and x y = (,) <$> x <*> y

  public export
  ||| Try both `x` and `y` and tag there results, then combine them
  or : (MonadChoice m, Alternative m) => m a -> m b -> m (Either a b)
  or x y = (x <&> Left) <|> (y <&> Right)

  public export 
  ||| Give all the values into one choice
  collect : (MonadChoice m, Applicative m, Alternative m) => m a -> m (List a)
  collect v = case !(look v) of 
    Nothing => pure []
    Just (x, xs) => pure (x :: (!(collect xs)))

  public export 
  ||| Give all values back to the choice
  split : (Foldable t, MonadChoice m, Alternative m) => m (t a) -> m a
  split t = let 
    l = toList <$> t
    in join $ foldr (\x => \y => (pure x <|> y)) fault <$> l

  public export 
  ||| Get all the values
  execChoice : (MonadChoice m, Traversable m, Alternative m) => m a -> List (m a)
  execChoice m = sequence $ collect m
