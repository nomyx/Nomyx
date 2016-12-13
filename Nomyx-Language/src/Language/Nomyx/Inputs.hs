{-# LANGUAGE GADTs #-}

-- | All the building blocks to allow rules to get inputs.
-- for example, you can create a button that will display a message like this:
-- do
--    void $ onInputButton_ "Click here:" (const $ outputAll_ "Bravo!") 1

module Language.Nomyx.Inputs (
   inputRadio, inputText, inputCheckbox, inputButton, inputTextarea,
   onInputRadio,    onInputRadio_,    onInputRadioOnce,
   onInputText,     onInputText_,     onInputTextOnce,
   onInputCheckbox, onInputCheckbox_, onInputCheckboxOnce,
   onInputButton,   onInputButton_,   onInputButtonOnce,
   onInputTextarea, onInputTextarea_, onInputTextareaOnce,
   ) where

import           Language.Nomyx.Types
import qualified Imprevu as Imp
import           Data.Typeable

-- * Inputs

-- ** Radio inputs

-- | event based on a radio input choice
inputRadio :: (Enum c) => PlayerNumber -> String -> [(c, String)] -> Event c
inputRadio = Imp.inputRadio

-- | triggers a choice input to the user. The result will be sent to the callback
onInputRadio :: (Enum a, Show a, Typeable a) => String -> [(a, String)] -> (EventNumber -> a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadio = Imp.onInputRadio

-- | the same, disregard the event number
onInputRadio_ :: (Enum a, Show a, Typeable a) => String -> [(a, String)] -> (a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadio_ = Imp.onInputRadio_

-- | the same, suppress the event after first trigger
onInputRadioOnce :: (Enum a, Show a, Typeable a) => String -> [(a, String)] -> (a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadioOnce = Imp.onInputRadioOnce

-- ** Text inputs

-- | event based on a text input
inputText :: PlayerNumber -> String -> Event String
inputText = Imp.inputText

-- | triggers a string input to the user. The result will be sent to the callback
onInputText :: String -> (EventNumber -> String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputText = Imp.onInputText

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputText_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputText_ = Imp.onInputText_

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputTextOnce :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextOnce = Imp.onInputTextOnce


-- ** Checkbox inputs

-- | event based on a checkbox input
inputCheckbox :: (Enum c) => PlayerNumber -> String -> [(c, String)] -> Event [c]
inputCheckbox = Imp.inputCheckbox

onInputCheckbox :: (Enum a, Show a, Typeable a) => String -> [(a, String)] -> (EventNumber -> [a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckbox = Imp.onInputCheckbox

onInputCheckbox_ :: (Enum a, Show a, Typeable a) => String -> [(a, String)] -> ([a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckbox_ = Imp.onInputCheckbox_

onInputCheckboxOnce :: (Enum a, Show a, Typeable a) => String -> [(a, String)] -> ([a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckboxOnce = Imp.onInputCheckboxOnce

-- ** Button inputs

-- | event based on a button
inputButton :: PlayerNumber -> String -> Event ()
inputButton = Imp.inputButton

onInputButton :: String -> (EventNumber -> () -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButton = Imp.onInputButton

onInputButton_ :: String -> (() -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButton_ = Imp.onInputButton_

onInputButtonOnce :: String -> (() -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButtonOnce = Imp.onInputButtonOnce


-- ** Textarea inputs

-- | event based on a text area
inputTextarea :: PlayerNumber -> String -> Event String
inputTextarea = Imp.inputTextarea

onInputTextarea :: String -> (EventNumber -> String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextarea = Imp.onInputTextarea

onInputTextarea_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextarea_ = Imp.onInputTextarea_

onInputTextareaOnce :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextareaOnce = Imp.onInputTextareaOnce


