module Data.Simple  
 
public export
Simple : Type -> Type
Simple t = t
  
public export 
Functor Simple where 
  map f = f 
  
public export 
Applicative Simple where 
  pure x = x
  (f <*> x) = f x
public export 
Monad Simple where 
  x >>= f = f x
  
public export 
Foldable Simple where 
  foldr f z x = f x z

public export 
Traversable Simple where
  traverse f x = (f x)
