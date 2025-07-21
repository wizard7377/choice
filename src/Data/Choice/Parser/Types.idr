module Data.Choice.Parser.Types
 
import Data.Choice.Types
import Data.Choice.Combinators
import Data.Choice.Instances as Instances
import Control.Monad.State
 
interface Eq tok => Stream s tok where 
  pushToken : tok -> s -> s
  popToken : s -> Maybe (tok, s)
  isEmpty : s -> Bool
  
Stream String Char where 
  pushToken t s = pack [t] ++ s
  popToken s = case unpack s of
    (t :: r) => Just (t, pack r)
    _ => Nothing
  isEmpty = null . unpack

interface MonadParser m where
Parser : (global : Type -> Type) -> (local : Type -> Type) -> (stream : Type) -> (error : Type) -> (action : Type) -> Type
Parser global local stream error action = StateT stream (ChoiceT global) (local (Either error action))

Monad global => Monad local => Stream stream _ => MonadParser (Parser global local stream error action) where
getToken : Monad g => Monad l => Stream s t => Parser g l s e t
getToken = do 
  state <- get
  case !(popToken state) of 
    Nothing => empty
    Just (x, xs) => 
      put xs
      pure x
  
