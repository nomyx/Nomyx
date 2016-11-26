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

inputRadio' :: (Enum a, Show a) => ClientNumber -> String -> [a] -> EventM n a
inputRadio' cn title cs = toEnum <$> SignalEvent (inputRadioSignal title (map (\a -> (fromEnum a, show a)) cs) cn)

-- | triggers a choice input to the user. The result will be sent to the callback
-- TODO: necessary Typeable?
onInputRadio :: (EvMgt n, Enum a, Show a, Typeable a) => String -> [a] -> (EventNumber -> a -> n ()) -> ClientNumber -> n EventNumber
onInputRadio title choices handler pn = onEvent (inputRadio' pn title choices) (\(en, a) -> handler en a)

-- | the same, disregard the event number
onInputRadio_ :: (EvMgt n, Enum a, Show a, Typeable a) => String -> [a] -> (a -> n ()) -> ClientNumber -> n EventNumber
onInputRadio_ title choices handler pn = onEvent_ (inputRadio' pn title choices) handler

-- | the same, suppress the event after first trigger
onInputRadioOnce :: (EvMgt n, Enum a, Show a, Typeable a) => String -> [a] -> (a -> n ()) -> ClientNumber -> n EventNumber
onInputRadioOnce title choices handler pn = onEventOnce (inputRadio' pn title choices) handler

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
inputCheckbox :: ClientNumber -> String -> [(Int, String)] -> EventM n [Int]
inputCheckbox cn title cs = SignalEvent (inputCheckboxSignal title cs cn)

inputCheckbox' :: (Enum a) => ClientNumber -> String -> [(a, String)] -> EventM n [Int]
inputCheckbox' cn title cs = SignalEvent (inputCheckboxSignal title (map (\(a, s) -> (fromEnum a, s)) cs) cn)

onInputCheckbox :: (EvMgt n) => String -> [(Int, String)] -> (EventNumber -> [Int] -> n ()) -> ClientNumber -> n EventNumber
onInputCheckbox title choices handler pn = onEvent (inputCheckbox pn title choices) (\(en, a) -> handler en a)

onInputCheckbox' :: (EvMgt n, Enum a) => String -> [(a, String)] -> (EventNumber -> [a] -> n ()) -> ClientNumber -> n EventNumber
onInputCheckbox' title choices handler pn = onEvent (inputCheckbox' pn title choices) (\(en, a) -> handler en (map toEnum a))

onInputCheckbox_ :: (EvMgt n) => String -> [(Int, String)] -> ([Int] -> n ()) -> ClientNumber -> n EventNumber
onInputCheckbox_ title choices handler pn = onEvent_ (inputCheckbox pn title choices) handler

onInputCheckboxOnce :: (EvMgt n) => String -> [(Int, String)] -> ([Int] -> n ()) -> ClientNumber -> n EventNumber
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

inputRadioSignal :: String -> [(Int, String)] -> ClientNumber -> Signal InputS Int
inputRadioSignal title cs cn = Signal (InputS (Radio title cs) cn)

inputTextSignal :: String -> ClientNumber -> Signal InputS String
inputTextSignal title cn = Signal (InputS (Text title) cn)

inputCheckboxSignal :: String -> [(Int, String)] -> ClientNumber -> Signal InputS [Int]
inputCheckboxSignal title cs cn = Signal (InputS (Checkbox title cs) cn)

inputButtonSignal :: String -> ClientNumber -> Signal InputS ()
inputButtonSignal title cn = Signal (InputS (Button title) cn)

inputTextareaSignal :: String -> ClientNumber -> Signal InputS String
inputTextareaSignal title cn = Signal (InputS (TextArea title) cn)

