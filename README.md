# Choice: The List Monad Transformer 

A List Monad Transformer for Idris, intended for real world use.

The interface for this consists of two main types, the `Choice` Monad transformer and the `MonadChoice` class.
The `Choice` Monad Transformer is based heavily off of the work done on the Haskell libaries `logict` and `list-t`, both of which it derives its form

> [!TIP]
> More in depth information may be found on the [Github pages](https://wizard7377.github.io/choice/)

## The basic internal structure

The `Choice` monad transformer, has essientally the following signature (this isn't how it's implemented, but it's close enough)
```idris
Choice : (Type -> Type) -> Type -> Type where 
    MkChoice : m (Maybe (a, Choice m a)) -> Choice m a
```
While this might seem strange at first, this is actually a somewhat simple coroutine.
First, let's look at a specific instance of this, for instance, this on `IO` is 
```idris
ChoiceIO : Type -> Type where 
    MkChoiceIO : IO (Maybe (a, ChoiceIO a)) -> ChoiceIO a
```
This is much clearer! Essentially, the `Choice` Monad allows us to get one result from the `Choice`, and then returns that value and the remaining `Choice` inside the inner monad. To recover the list of choices, we simply run this until `Choice` returns nothing
