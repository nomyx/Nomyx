
-- | This file gives a list of example rules that the players can submit.
module Nomyx.Library.PlayerManagement where

import Control.Monad
import Language.Nomyx

-- | kick a player and prevent him from returning
banPlayer :: PlayerNumber -> Rule
banPlayer pn = do
   delPlayer pn
   void $ onEvent_ (playerEvent Arrive) $ const $ void $ delPlayer pn--- | kick a player and prevent him from returning

