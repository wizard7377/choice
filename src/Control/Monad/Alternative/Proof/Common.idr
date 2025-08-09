module Control.Monad.Alternative.Proof.Common

public export
record WfIsomorphism (a : Type) (b : Type) where
  constructor MkWfIsomorphism
  proj1 : a -> b 
  proj2 : b -> a 
  invProp1 : (x : a) -> (proj2 . proj1) x === x
  invProp2 : (y : b) -> (proj1 . proj2) y === y