module Test.Parser
  
import Data.Choice
import Control.Monad.State
  
ParserT : (Type -> Type) -> Type -> Type
ParserT e m a = StateT String (ChoiceT m) (Either e a)

take : Monad m => ParserT m Char
poke : Monad m => Char -> ParserT m ()
 
take = do
  state <- get
  case unpack state of 
    (c :: r) => do 
      put $ pack r
      pure c
    _ => empty
  
poke c = do 
  state <- get 
  put $ pack (c :: (unpack state))
