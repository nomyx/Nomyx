{-# LANGUAGE GADTs #-}

-- | All the building blocks to allow rules to get inputs.
-- for example, you can create a button that will display a message like this:
-- do
--    void $ onInputButton_ "Click here:" (const $ outputAll_ "Bravo!") 1

module Language.Nomyx.Inputs (
   Input(..),
   InputForm(..),
   onInputRadio,    onInputRadio_,    onInputRadioOnce,
   onInputText,     onInputText_,     onInputTextOnce,
   onInputCheckbox, onInputCheckbox_, onInputCheckboxOnce,
   onInputButton,   onInputButton_,   onInputButtonOnce,
   onInputTextarea, onInputTextarea_, onInputTextareaOnce,
   -- Internals
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
onInputRadio title choices handler pn = onEvent (inputRadioHead pn title choices) (\(en, InputData (RadioData a)) -> handler en a)

-- | the same, disregard the event number
onInputRadio_ :: (Typeable a, Eq a, Show a) => String -> [a] -> (a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadio_ title choices handler pn = onEvent_ (inputRadioHead pn title choices) (handler . inputRadioData)

-- | the same, suppress the event after first trigger
onInputRadioOnce :: (Typeable a, Eq a, Show a) => String -> [a] -> (a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadioOnce title choices handler pn = onEventOnce (inputRadioHead pn title choices) (handler . inputRadioData)

inputRadio :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [c] -> c -> Event (Input c)
inputRadio pn title cs _ = InputEv (Input pn title (Radio (zip cs (show <$> cs))))

inputRadioHead :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [c] -> Event (Input c)
inputRadioHead pn title choices = inputRadio pn title choices (head choices)

inputRadioData :: (Show c) => EventData (Input c) -> c
inputRadioData (InputData (RadioData a)) = a
inputRadioData a = error $ "Not a Radio Data: " ++ show a
-- ** Text inputs

-- | triggers a string input to the user. The result will be sent to the callback
onInputText :: String -> (EventNumber -> String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputText title handler pn = onEvent (inputText pn title) (\(en, a) -> handler en (inputTextData a))

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputText_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputText_ title handler pn = onEvent_ (inputText pn title) (handler . inputTextData)

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputTextOnce :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextOnce title handler pn = onEventOnce (inputText pn title) (handler . inputTextData)

inputText :: PlayerNumber -> String -> Event (Input String)
inputText pn title = InputEv (Input pn title Text)

inputTextData :: EventData (Input String) -> String
inputTextData (InputData (TextData a)) = a
inputTextData a = error $ "Not a Text Data: " ++ (show a)


-- ** Checkbox inputs

onInputCheckbox :: (Typeable a, Eq a,  Show a) => String -> [(a, String)] -> (EventNumber -> [a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckbox title choices handler pn = onEvent (inputCheckbox pn title choices) (\(en, InputData (CheckboxData a)) -> handler en a)

onInputCheckbox_ :: (Typeable a, Eq a,  Show a) => String -> [(a, String)] -> ([a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckbox_ title choices handler pn = onEvent_ (inputCheckbox pn title choices) (handler . inputCheckboxData)

onInputCheckboxOnce :: (Typeable a, Eq a,  Show a) => String -> [(a, String)] -> ([a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckboxOnce title choices handler pn = onEventOnce (inputCheckbox pn title choices) (handler . inputCheckboxData)

inputCheckboxData :: (Show c) => EventData (Input c) -> [c]
inputCheckboxData (InputData (CheckboxData a)) = a
inputCheckboxData a = error $ "Not a Checkbox Data: " ++ show a

inputCheckbox :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [(c, String)] -> Event (Input c)
inputCheckbox pn title cs = InputEv (Input pn title (Checkbox cs))

-- ** Button inputs

onInputButton :: String -> (EventNumber -> () -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButton title handler pn = onEvent (inputButton pn title) (\(en, InputData ButtonData) -> handler en ())

onInputButton_ :: String -> (() -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButton_ title handler pn = onEvent_ (inputButton pn title) (handler . inputButtonData)

onInputButtonOnce :: String -> (() -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButtonOnce title handler pn = onEventOnce (inputButton pn title) (handler . inputButtonData)

inputButtonData :: EventData (Input ()) -> ()
inputButtonData (InputData ButtonData) = ()
inputButtonData a = error $ "Not a Button Data: " ++ show a

inputButton :: PlayerNumber -> String -> Event (Input ())
inputButton pn title = InputEv (Input pn title Button)

-- ** Textarea inputs

inputTextarea :: PlayerNumber -> String -> Event (Input String)
inputTextarea pn title = InputEv (Input pn title TextArea)

onInputTextarea :: String -> (EventNumber -> String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextarea title handler pn = onEvent (inputTextarea pn title) (\(en, a) -> handler en (inputTextareaData a))

onInputTextarea_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextarea_ title handler pn = onEvent_ (inputTextarea pn title) (handler . inputTextareaData)

onInputTextareaOnce :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextareaOnce title handler pn = onEventOnce (inputTextarea pn title) (handler . inputTextareaData)

inputTextareaData :: EventData (Input String) -> String
inputTextareaData (InputData (TextAreaData a)) = a
inputTextareaData a = error $ "Not a Textarea Data: " ++ show a
