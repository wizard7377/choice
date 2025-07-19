module Data.Choice.Combinators 

import Data.Choice.Types
import Data.Choice.Instances
  
  
namespace C 
  public export
  andThen : MonadChoice m => m a -> (a -> m b) -> m b
  andThen = (>>=)
  public export
  and : MonadChoice m => m a -> m b -> m (a, b)
  and x y = (,) <$> x <*> y
  public export
  or : (MonadChoice m, Alternative m) => m a -> m b -> m (Either a b)
  or x y = (x <&> Left) <|> (y <&> Right)
  public export 
  split : (MonadChoice m, Alternative m) => m (List a) -> m a
  split l = case !l of 
    [] => give $ pure Nothing
    (x :: xs) => give $ pure $ Just (x, split $ pure xs)
  public export 
  collect : (MonadChoice m, Applicative m, Alternative m) => m a -> m (List a)
  collect v = case !(look v) of 
    Nothing => pure []
    Just (x, xs) => pure (x :: (!(collect xs)))
  public export 
  select : (Foldable t, MonadChoice m) => t a -> m a
  select t = let 
    l = toList t
    in ?s
