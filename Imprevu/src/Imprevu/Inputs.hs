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
import Imprevu.Event
import Data.Typeable

type ClientNumber = Int


-- * Inputs

-- ** Radio inputs

-- | event based on a radio input choice
inputRadio :: (Eq c, Show c, Typeable c) => ClientNumber -> String -> [(c, String)] -> Event n c
inputRadio pn title cs = inputEvent $ Radio title cs

inputRadio' :: (Eq c, Show c, Typeable c) => ClientNumber -> String -> [c] -> Event n c
inputRadio' pn title cs = inputEvent $ Radio title (zip cs (show <$> cs))

-- | triggers a choice input to the user. The result will be sent to the callback
onInputRadio :: (Typeable a, Eq a,  Show a, EvMgt n) => String -> [a] -> (EventNumber -> a -> n ()) -> ClientNumber -> n EventNumber
onInputRadio title choices handler pn = onEvent (inputRadio' pn title choices) (\(en, a) -> handler en a)

-- | the same, disregard the event number
onInputRadio_ :: (Typeable a, Eq a, Show a, EvMgt n) => String -> [a] -> (a -> n ()) -> ClientNumber -> n EventNumber
onInputRadio_ title choices handler pn = onEvent_ (inputRadio' pn title choices) handler

-- | the same, suppress the event after first trigger
onInputRadioOnce :: (Typeable a, Eq a, Show a, EvMgt n) => String -> [a] -> (a -> n ()) -> ClientNumber -> n EventNumber
onInputRadioOnce title choices handler pn = onEventOnce (inputRadio' pn title choices) handler

-- ** Text inputs

-- | event based on a text input
inputText :: ClientNumber -> String -> Event n String
inputText pn title = inputEvent $ inputTextSignal pn title

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
inputCheckbox :: (Eq c, Show c, Typeable c) => ClientNumber -> String -> [(c, String)] -> Event n [c]
inputCheckbox pn title cs = inputEvent $ inputCheckboxSignal pn title cs

onInputCheckbox :: (Typeable a, Eq a,  Show a, EvMgt n) => String -> [(a, String)] -> (EventNumber -> [a] -> n ()) -> ClientNumber -> n EventNumber
onInputCheckbox title choices handler pn = onEvent (inputCheckbox pn title choices) (\(en, a) -> handler en a)

onInputCheckbox_ :: (Typeable a, Eq a,  Show a, EvMgt n) => String -> [(a, String)] -> ([a] -> n ()) -> ClientNumber -> n EventNumber
onInputCheckbox_ title choices handler pn = onEvent_ (inputCheckbox pn title choices) handler

onInputCheckboxOnce :: (Typeable a, Eq a, Show a, EvMgt n) => String -> [(a, String)] -> ([a] -> n ()) -> ClientNumber -> n EventNumber
onInputCheckboxOnce title choices handler pn = onEventOnce (inputCheckbox pn title choices) handler

-- ** Button inputs

-- | event based on a button
inputButton :: ClientNumber -> String -> Event n ()
inputButton pn title = inputEvent $ inputButtonSignal pn title

onInputButton :: (EvMgt n) => String -> (EventNumber -> () -> n ()) -> ClientNumber -> n EventNumber
onInputButton title handler pn = onEvent (inputButton pn title) (\(en, ()) -> handler en ())

onInputButton_ :: (EvMgt n) => String -> (() -> n ()) -> ClientNumber -> n EventNumber
onInputButton_ title handler pn = onEvent_ (inputButton pn title) handler

onInputButtonOnce :: (EvMgt n) => String -> (() -> n ()) -> ClientNumber -> n EventNumber
onInputButtonOnce title handler pn = onEventOnce (inputButton pn title) handler


-- ** Textarea inputs

-- | event based on a text area
inputTextarea :: ClientNumber -> String -> Event n String
inputTextarea pn title = inputEvent $ inputTextareaSignal pn title

onInputTextarea :: (EvMgt n) => String -> (EventNumber -> String -> n ()) -> ClientNumber -> n EventNumber
onInputTextarea title handler pn = onEvent (inputTextarea pn title) (\(en, a) -> handler en a)

onInputTextarea_ :: (EvMgt n) => String -> (String -> n ()) -> ClientNumber -> n EventNumber
onInputTextarea_ title handler pn = onEvent_ (inputTextarea pn title) handler

onInputTextareaOnce :: (EvMgt n) => String -> (String -> n ()) -> ClientNumber -> n EventNumber
onInputTextareaOnce title handler pn = onEventOnce (inputTextarea pn title) handler


-- ** Internals

inputRadioSignal :: (Eq c, Show c, Typeable c) => ClientNumber -> String -> [(c, String)] -> Input c
inputRadioSignal pn title cs = Radio title cs

inputTextSignal :: ClientNumber -> String -> Input String
inputTextSignal pn title = Text title

inputCheckboxSignal :: (Eq c, Show c, Typeable c) => ClientNumber -> String -> [(c, String)] -> Input [c]
inputCheckboxSignal pn title cs = Checkbox title cs

inputButtonSignal :: ClientNumber -> String -> Input ()
inputButtonSignal pn title = Button title

inputTextareaSignal :: ClientNumber -> String -> Input String
inputTextareaSignal pn title = TextArea title

