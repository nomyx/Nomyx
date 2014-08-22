{-# LANGUAGE GADTs #-}

-- | All the building blocks to allow rules to get inputs.
-- for example, you can create a button that will display a message like this:
-- do
--    void $ onInputButton_ "Click here:" (const $ outputAll_ "Bravo!") 1

module Language.Nomyx.Inputs (
   InputForm(..),
   onInputRadio,    onInputRadio_,    onInputRadioOnce,
   onInputText,     onInputText_,     onInputTextOnce,
   onInputCheckbox, onInputCheckbox_, onInputCheckboxOnce,
   onInputButton,   onInputButton_,   onInputButtonOnce,
   onInputTextarea, onInputTextarea_, onInputTextareaOnce,
   -- Internals
   baseInputRadio, baseInputText, baseInputCheckbox, baseInputButton, baseInputTextarea,
   inputRadio, inputText, inputCheckbox, inputButton, inputTextarea
   ) where

import Language.Nomyx.Expression
import Language.Nomyx.Events
import Data.Typeable
import Control.Applicative

-- * Inputs

-- ** Radio inputs

-- | triggers a choice input to the user. The result will be sent to the callback
onInputRadio :: (Typeable a, Eq a,  Show a) => String -> [a] -> (EventNumber -> a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadio title choices handler pn = onEvent (return $ inputRadio' pn title choices) (\(en, a) -> handler en a)

-- | the same, disregard the event number
onInputRadio_ :: (Typeable a, Eq a, Show a) => String -> [a] -> (a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadio_ title choices handler pn = onEvent_ (return $ inputRadio' pn title choices) handler

-- | the same, suppress the event after first trigger
onInputRadioOnce :: (Typeable a, Eq a, Show a) => String -> [a] -> (a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadioOnce title choices handler pn = onEventOnce (return $ inputRadio' pn title choices) handler

inputRadio :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [(c, String)] -> Event c
inputRadio pn title cs = baseEvent $ baseInputRadio pn title cs

inputRadio' :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [c] -> Event c
inputRadio' pn title cs = inputRadio pn title (zip cs (show <$> cs))

baseInputRadio :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [(c, String)] -> Field c
baseInputRadio pn title cs = baseInputEvent pn title (Radio cs)

-- ** Text inputs

-- | triggers a string input to the user. The result will be sent to the callback
onInputText :: String -> (EventNumber -> String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputText title handler pn = onEvent (return $ inputText pn title) (\(en, a) -> handler en a)

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputText_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputText_ title handler pn = onEvent_ (return $ inputText pn title) handler

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputTextOnce :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextOnce title handler pn = onEventOnce (return $ inputText pn title) handler

inputText :: PlayerNumber -> String -> Event String
inputText pn title = baseEvent $ baseInputText pn title

baseInputText :: PlayerNumber -> String -> Field String
baseInputText pn title = baseInputEvent pn title Text

-- ** Checkbox inputs

onInputCheckbox :: (Typeable a, Eq a,  Show a) => String -> [(a, String)] -> (EventNumber -> [a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckbox title choices handler pn = onEvent (return $ inputCheckbox pn title choices) (\(en, a) -> handler en a)

onInputCheckbox_ :: (Typeable a, Eq a,  Show a) => String -> [(a, String)] -> ([a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckbox_ title choices handler pn = onEvent_ (return $ inputCheckbox pn title choices) handler

onInputCheckboxOnce :: (Typeable a, Eq a,  Show a) => String -> [(a, String)] -> ([a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckboxOnce title choices handler pn = onEventOnce (return $ inputCheckbox pn title choices) handler

inputCheckbox :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [(c, String)] -> Event [c]
inputCheckbox pn title cs = baseEvent $ baseInputCheckbox pn title cs

baseInputCheckbox :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [(c, String)] -> Field [c]
baseInputCheckbox pn title cs = baseInputEvent pn title (Checkbox cs)

-- ** Button inputs

onInputButton :: String -> (EventNumber -> () -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButton title handler pn = onEvent (return $ inputButton pn title) (\(en, ()) -> handler en ())

onInputButton_ :: String -> (() -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButton_ title handler pn = onEvent_ (return $ inputButton pn title) handler

onInputButtonOnce :: String -> (() -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButtonOnce title handler pn = onEventOnce (return $ inputButton pn title) handler

inputButton :: PlayerNumber -> String -> Event ()
inputButton pn title = baseEvent $ baseInputButton pn title

baseInputButton :: PlayerNumber -> String -> Field ()
baseInputButton pn title = baseInputEvent pn title Button

-- ** Textarea inputs

onInputTextarea :: String -> (EventNumber -> String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextarea title handler pn = onEvent (return $ inputTextarea pn title) (\(en, a) -> handler en a)

onInputTextarea_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextarea_ title handler pn = onEvent_ (return $ inputTextarea pn title) handler

onInputTextareaOnce :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextareaOnce title handler pn = onEventOnce (return $ inputTextarea pn title) handler

inputTextarea :: PlayerNumber -> String -> Event String
inputTextarea pn title = baseEvent $ baseInputTextarea pn title

baseInputTextarea :: PlayerNumber -> String -> Field String
baseInputTextarea pn title = baseInputEvent pn title TextArea
