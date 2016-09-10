{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | This file gives a list of example rules that the players can submit.
module Nomyx.Library.Examples where

import Prelude
import Control.Monad as X
import Language.Nomyx


-- | A rule that does nothing
nothing :: Rule
nothing = return ()

-- | A rule that says hello to all players
helloWorld :: Rule
helloWorld = outputAll_ "hello, world!"

-- | delete a rule
delRule :: RuleNumber -> Rule
delRule rn = suppressRule_ rn >> autoDelete

-- | will display the current time (when refreshing the screen)
displayCurrentTime :: Rule
displayCurrentTime = void $ outputAll $ do
    t <- getCurrentTime
    return $ "The current time is: " ++ (show t)

-- | will display the time at which the rule as been activated
displayActivateTime :: Nomex ()
displayActivateTime = do
   time <- getCurrentTime
   outputAll_ $ "This rule was activated at: " ++ (show time)

-- | display a button and greets you when pressed (for player 1)
bravoButton :: Rule
bravoButton = void $ onInputButton_ "Click here:" (const $ outputAll_ "Bravo!") 1

-- | display a button to greet other players
helloButton :: Rule
helloButton = do
   --get your own player number
   me <- getProposerNumber_
   --create an output for me only
   let displayMsg a = void $ newOutput_ Nothing ("Msg: " ++ a)
   --create a button for me, which will display the output when clicked
   let button = do
       all <- liftEvent getPlayers
       guard (length all >= 2) >> inputText me "send a message"
   void $ onEvent_ button displayMsg

enterHaiku :: Rule
enterHaiku = void $ onInputTextarea_ "Enter a haiku:" outputAll_ 1

