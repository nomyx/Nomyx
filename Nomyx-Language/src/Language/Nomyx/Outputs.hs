-- All the building blocks to allow rules to produce outputs.
-- for example, you can display a message like this:
-- do
--    outputAll_ "hello, world!"

module Language.Nomyx.Outputs (
   OutputNumber,
   newOutput, newOutput_,
   outputAll, outputAll_,
   getOutput, getOutput_,
   updateOutput,
   delOutput,
   displayVar, displayVar',
   displaySimpleVar
   ) where

import Language.Nomyx.Types
import Imprevu
import Data.Typeable
import Control.Monad.State


-- * Outputs

-- | outputs a message to one player, dynamic version
newOutput :: Maybe PlayerNumber -> Nomex String -> Nomex OutputNumber
newOutput = NewOutput

-- | outputs a message to one player, static version
newOutput_ :: Maybe PlayerNumber -> String -> Nomex OutputNumber
newOutput_ ns mpn = NewOutput ns (return mpn)

-- | output a message to all players
outputAll :: Nomex String -> Nomex OutputNumber
outputAll = newOutput Nothing

-- | output a constant message to all players
outputAll_ :: String -> Nomex ()
outputAll_ s = void $ newOutput Nothing (return s)

-- | get an output by number
getOutput :: OutputNumber -> Nomex (Maybe String)
getOutput = GetOutput

-- | get an output by number, partial version
getOutput_ :: OutputNumber -> Nomex String
getOutput_ on = partial "getOutput_ : Output number not existing" $ getOutput on

-- | update an output
updateOutput :: OutputNumber -> Nomex String -> Nomex Bool
updateOutput = UpdateOutput

-- | delete an output
delOutput :: OutputNumber -> Nomex Bool
delOutput = DelOutput

-- | display a variable
displayVar :: (Typeable a, Show a) => Maybe PlayerNumber -> V a -> (Maybe a -> Nomex String) -> Nomex OutputNumber
displayVar mpn mv dis = newOutput mpn $ readVar mv >>= dis

-- permanently display a variable
displayVar' :: (Typeable a, Show a) => Maybe PlayerNumber -> V a -> (a -> Nomex String) -> Nomex OutputNumber
displayVar' mpn mv dis = displayVar mpn mv dis' where
   dis' Nothing  = return $ "Variable " ++ (varName mv) ++ " deleted"
   dis' (Just a) = (++ "\n") <$> dis a

displaySimpleVar :: (Typeable a, Show a) => Maybe PlayerNumber -> V a -> String -> Nomex OutputNumber
displaySimpleVar mpn mv title = displayVar' mpn mv showVar where
   showVar a = return $ title ++ ": " ++ (show a) ++ "\n"
