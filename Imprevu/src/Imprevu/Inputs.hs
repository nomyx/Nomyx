{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | All the building blocks to allow rules to get inputs.
module Imprevu.Inputs
--   InputForm(..),
--   inputRadio, inputText, inputCheckbox, inputButton, inputTextarea,
--   onInputRadio,    onInputRadio_,    onInputRadioOnce, inputRadio',
--   onInputText,     onInputText_,     onInputTextOnce,
--   onInputCheckbox, onInputCheckbox_, onInputCheckboxOnce,
--   onInputButton,   onInputButton_,   onInputButtonOnce,
--   onInputTextarea, onInputTextarea_, onInputTextareaOnce,
    where

import Imprevu.Events
import Imprevu.Types
import Data.Typeable

-- * Inputs

-- ** Radio inputs

-- | event based on a radio input choice
inputRadio :: (Enum a) => ClientNumber -> String -> [(a, String)] -> EventM n a
inputRadio cn title cs = toEnum <$> SignalEvent (inputRadioSignal title (map (\(a, s) -> (fromEnum a, s)) cs) cn)

-- | triggers a choice input to the user. The result will be sent to the callback
-- TODO: necessary Typeable?
onInputRadio :: (EvMgt n, Enum a, Show a, Typeable a) => String -> [(a, String)] -> (EventNumber -> a -> n ()) -> ClientNumber -> n EventNumber
onInputRadio title choices handler pn = onEvent (inputRadio pn title choices) (\(en, a) -> handler en a)

-- | the same, disregard the event number
onInputRadio_ :: (EvMgt n, Enum a, Show a, Typeable a) => String -> [(a, String)] -> (a -> n ()) -> ClientNumber -> n EventNumber
onInputRadio_ title choices handler pn = onEvent_ (inputRadio pn title choices) handler

-- | the same, suppress the event after first trigger
onInputRadioOnce :: (EvMgt n, Enum a, Show a, Typeable a) => String -> [(a, String)] -> (a -> n ()) -> ClientNumber -> n EventNumber
onInputRadioOnce title choices handler pn = onEventOnce (inputRadio pn title choices) handler

-- ** Text inputs

-- | event based on a text input
inputText :: ClientNumber -> String -> EventM n String
inputText cn title = SignalEvent (inputTextSignal title cn)

-- | triggers a string input to the user. The result will be sent to the callback
onInputText :: (EvMgt n) => String -> (EventNumber -> String -> n ()) -> ClientNumber -> n EventNumber
onInputText title handler pn = onEvent (inputText pn title) (\(en, a) -> handler en a)

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputText_ :: (EvMgt n) => String -> (String -> n ()) -> ClientNumber -> n EventNumber
onInputText_ title handler pn = onEvent_ (inputText pn title) handler

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputTextOnce :: (EvMgt n) => String -> (String -> n ()) -> ClientNumber -> n EventNumber
onInputTextOnce title handler pn = onEventOnce (inputText pn title) handler


-- ** Checkbox inputs

-- | event based on a checkbox input
inputCheckbox :: (Enum a) => ClientNumber -> String -> [(a, String)] -> EventM n [a]
inputCheckbox cn title cs = (map toEnum) <$> SignalEvent (inputCheckboxSignal title (map (\(a, s) -> (fromEnum a, s)) cs) cn)

onInputCheckbox :: (EvMgt n, Enum a, Show a, Typeable a) => String -> [(a, String)] -> (EventNumber -> [a] -> n ()) -> ClientNumber -> n EventNumber
onInputCheckbox title choices handler pn = onEvent (inputCheckbox pn title choices) (\(en, a) -> handler en a)

onInputCheckbox_ :: (EvMgt n, Enum a, Show a, Typeable a) => String -> [(a, String)] -> ([a] -> n ()) -> ClientNumber -> n EventNumber
onInputCheckbox_ title choices handler pn = onEvent_ (inputCheckbox pn title choices) handler

onInputCheckboxOnce :: (EvMgt n, Enum a, Show a, Typeable a) => String -> [(a, String)] -> ([a] -> n ()) -> ClientNumber -> n EventNumber
onInputCheckboxOnce title choices handler pn = onEventOnce (inputCheckbox pn title choices) handler

-- ** Button inputs

-- | event based on a button
inputButton :: ClientNumber -> String -> EventM n ()
inputButton cn title = SignalEvent (inputButtonSignal title cn)

onInputButton :: (EvMgt n) => String -> (EventNumber -> () -> n ()) -> ClientNumber -> n EventNumber
onInputButton title handler pn = onEvent (inputButton pn title) (\(en, ()) -> handler en ())

onInputButton_ :: (EvMgt n) => String -> (() -> n ()) -> ClientNumber -> n EventNumber
onInputButton_ title handler pn = onEvent_ (inputButton pn title) handler

onInputButtonOnce :: (EvMgt n) => String -> (() -> n ()) -> ClientNumber -> n EventNumber
onInputButtonOnce title handler pn = onEventOnce (inputButton pn title) handler


-- ** Textarea inputs

-- | event based on a text area
inputTextarea :: ClientNumber -> String -> EventM n String
inputTextarea cn title = SignalEvent (inputTextareaSignal title cn)

onInputTextarea :: (EvMgt n) => String -> (EventNumber -> String -> n ()) -> ClientNumber -> n EventNumber
onInputTextarea title handler pn = onEvent (inputTextarea pn title) (\(en, a) -> handler en a)

onInputTextarea_ :: (EvMgt n) => String -> (String -> n ()) -> ClientNumber -> n EventNumber
onInputTextarea_ title handler pn = onEvent_ (inputTextarea pn title) handler

onInputTextareaOnce :: (EvMgt n) => String -> (String -> n ()) -> ClientNumber -> n EventNumber
onInputTextareaOnce title handler pn = onEventOnce (inputTextarea pn title) handler


-- ** Internals

inputRadioSignal :: String -> [(Int, String)] -> ClientNumber -> Signal Input Int
inputRadioSignal title cs cn = Signal (Input (Radio title cs) cn)

inputTextSignal :: String -> ClientNumber -> Signal Input String
inputTextSignal title cn = Signal (Input (Text title) cn)

inputCheckboxSignal :: String -> [(Int, String)] -> ClientNumber -> Signal Input [Int]
inputCheckboxSignal title cs cn = Signal (Input (Checkbox title cs) cn)

inputButtonSignal :: String -> ClientNumber -> Signal Input ()
inputButtonSignal title cn = Signal (Input (Button title) cn)

inputTextareaSignal :: String -> ClientNumber -> Signal Input String
inputTextareaSignal title cn = Signal (Input (TextArea title) cn)

