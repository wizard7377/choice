module Data.Choice.Combinators 

import Data.Choice.Types
import Data.Choice.Instances
  
  
namespace C 

  ||| ⊥
  public export 
  fault : MonadChoice m => m a
  fault = give $ pure Nothing

  ||| Get a value, and then run another computation
  ||| Equivalent to monadic `>>=`
  public export
  andThen : MonadChoice m => m a -> (a -> m b) -> m b
  andThen = (>>=)
  
  public export
  ||| Conjunction of two statements, equivalent to `(,)` over the monad
  and : MonadChoice m => m a -> m b -> m (a, b)
  and x y = (,) <$> x <*> y

  ||| Try both `x` and `y` and tag there results, then combine them
  public export
  or : (MonadChoice m, Alternative m) => m a -> m b -> m (Either a b)
  or x y = (x <&> Left) <|> (y <&> Right)

  ||| Give all the values into one choice
  public export 
  collect : (MonadChoice m, Applicative m, Alternative m) => m a -> m (List a)
  collect v = case !(look v) of 
    Nothing => pure []
    Just (x, xs) => pure (x :: (!(collect xs)))

  ||| Give all values back to the choice
  public export 
  split : (Foldable t, MonadChoice m) => (Alternative m) => m (t a) -> m a
  split t = let 
    l = toList <$> t
    in join $ foldr (\x => \y => (pure x <|> y)) fault <$> l

  ||| Get all the values
  public export 
  execChoice : (MonadChoice m, Traversable m, Alternative m) => m a -> List (m a)
  execChoice m = sequence $ collect m
  
  ||| Try the choice, if it yields nothing, return exactly one default value
  public export
  recover : MonadChoice m => (_ : a) -> (_ : m a) -> m a
  recover v c = case !(look c) of
    Nothing => pure v
    _ => c
