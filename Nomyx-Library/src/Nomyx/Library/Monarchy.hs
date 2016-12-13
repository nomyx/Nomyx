
-- | This file gives a list of example rules that the players can submit.
module Nomyx.Library.Monarchy where

import Control.Monad
import Nomyx.Language

-- | Variable holding the player number of the King
king :: V PlayerNumber
king = V "King"

-- | player pn is the king: we create a variable King to identify him,
-- and we prefix his name with "King"
makeKing :: PlayerNumber -> Rule
makeKing pn = do
   newVar_ "King" pn
   void $ modifyPlayerName pn ("King " ++)

-- | Monarchy: only the king decides which rules to accept or reject
monarchy :: PlayerNumber -> Rule
monarchy pn = do
   makeKing pn
   void $ onEvent_ (ruleEvent Proposed) $ \rule -> do
      k <- readVar_ king
      void $ onInputRadioOnce ("Your Royal Highness, do you accept rule " ++ (show $ _rNumber rule) ++ "?") [(True, "Yes"), (False, "No")] (activateOrRejectRule rule) k

-- | Revolution! Hail to the king!
-- This rule suppresses the democracy (usually rules 1 and 2), installs the king and activates monarchy.
revolution :: PlayerNumber -> Rule
revolution pn = do
    suppressRule 1
    rNum <- addRule' "Monarchy" (monarchy pn) ("monarchy " ++ (show pn)) "Monarchy: only the king can vote on new rules"
    activateRule_ rNum
    autoDelete

