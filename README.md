# Choice: The List Monad Transformer 

> [!CAUTION]
> Development has been moved to [idris-logic](https://github.com/wizard7377/idris-logic)

A List Monad Transformer for Idris, intended for real world use.

The interface for this consists of two main types, the `Choice` Monad transformer and the `MonadChoice` class.
The `Choice` Monad Transformer is based heavily off of the work done on the Haskell libaries `logict` and `list-t`, both of which it derives its form

> [!TIP]
> More in depth information may be found on the [Github pages](https://wizard7377.github.io/choice/)

## Combinatory logic, or the Theory of Everything 

The abilty of `ChoiceT` is that of being able to wrap around *global* state. That is, `ChoiceT (State s) a` represents a computation with a *global* state `s`. If we wanted to add a local state, we would then do `StateT t (ChoiceT (State s)) a`. 

TODO: Needs a better example

## 
The ability of monads to allow us to consider the context of values "as we need them", is quite useful. Indeed, monads like `State` and `Writer` allow us to avoid writing impure code by cleverly using pure code inside a monad.

There are many monads that exist, from `Parser`s, to `Maybe`, there are really quite a few. However, one of the most useful properties of most monads is their ability to *transform*. That is, allow us to reason about a monadic `State` with a monadic `Output` together.

However, there is one monad that lacks this ability, at least on its surface: the `List` monad. While 
