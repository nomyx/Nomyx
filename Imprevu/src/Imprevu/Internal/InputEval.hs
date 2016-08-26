{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Evaluation of the events
module Imprevu.Internal.InputEval where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.List
import           Data.Typeable
import           Data.Maybe
import           Data.Validation
import           Imprevu.Internal.Event
import           Imprevu.Internal.EventEval
import           Imprevu.Internal.Utils
import           Prelude                     hiding (log, (.))
import           GHC.Generics
import           Safe

-- a form field with its title
data InputView = RadioField    String [(Int, String)]
               | TextField     String
               | TextAreaField String
               | ButtonField   String
               | CheckboxField String [(Int, String)]
               deriving (Show, Read, Ord, Eq, Generic, Typeable)

-- data sent back by the form fields
data InputDataView = RadioData    Int
               | CheckboxData [Int]
               | TextData     String
               | TextAreaData String
               | ButtonData
               deriving (Show, Read, Eq, Ord, Typeable)

--View an Input
viewSignal :: Input a -> InputView
viewSignal (Radio s cs)    = (RadioField s (zip [0..] (snd <$> cs)))
viewSignal (Checkbox s cs) = (CheckboxField s (zip [0..] (snd <$> cs)))
viewSignal (Text s)        = (TextField s)
viewSignal (TextArea s)    = (TextAreaField s)
viewSignal (Button s)      = (ButtonField s)

 -- act an Input
actSignal :: (Show a) => Input a -> InputDataView -> a
actSignal (Radio _ cs)    (RadioData i) = fst $ cs !! i
actSignal (Checkbox _ cs) (CheckboxData is) = fst <$> cs `sel` is
actSignal (Text _)        (TextData d) = d
actSignal (TextArea _)    (TextAreaData d) = d
actSignal (Button _)      (ButtonData) = ()
actSignal _ _ = error "actSignal"

-- * Input triggers

-- trigger the input form with the input data
triggerInput :: (HasEvents n s) => InputView -> InputDataView -> SignalAddress -> EventNumber -> Evaluate n s ()
triggerInput sv dv sa en = do
   evs <- use events
   let mei = find (\a -> a ^. eventNumber == en) evs
   mapM_ (triggerInputSignal sv dv sa) mei

-- trigger the input signal with the input data
triggerInputSignal :: forall n s. (HasEvents n s) => InputView -> InputDataView -> SignalAddress -> EventInfo n -> Evaluate n s ()
triggerInputSignal sv dv sa ei@(EventInfo _ _ _ SActive _) = do
    mss <- findField sv sa ei
    case mss of
       Just (SomeSignal (InputS e')) -> triggerEvent' (SignalData (InputS e') (actSignal e' dv))  (Just sa) [ei]
       _ -> error $ "Input not found"
triggerInputSignal _ _ _ _ = return ()

-- | Get the form field at a certain address
findField :: InputView -> SignalAddress -> EventInfo n -> Evaluate n s (Maybe SomeSignal)
findField sv sa (EventInfo _ e _ _ envi) = findField' sa e envi sv

findField' :: SignalAddress -> Event e -> [SignalOccurence] -> InputView -> Evaluate n s (Maybe SomeSignal)
findField' []         (SignalEvent f)    _   ff = return $ do
   --ff' <- getFormField f
   --guard (ff' == ff)
   return $ SomeSignal f
findField' (SumL:as)  (SumEvent e1 _)  envi ff = findField' as e1 (filterPath SumL envi) ff
findField' (SumR:as)  (SumEvent _ e2)  envi ff = findField' as e2 (filterPath SumR envi) ff
findField' (AppL:as)  (AppEvent e1 _)  envi ff = findField' as e1 (filterPath AppL envi) ff
findField' (AppR:as)  (AppEvent _ e2)  envi ff = findField' as e2 (filterPath AppR envi) ff
findField' (BindL:as) (BindEvent e1 _) envi ff = findField' as e1 (filterPath BindL envi) ff
findField' (BindR:as) (BindEvent e1 f) envi ff = do
   ter <- getEventResult e1 (filterPath BindL envi)
   case ter of
      AccSuccess e2 -> findField' as (f e2) (filterPath BindR envi) ff
      AccFailure _  -> return Nothing
findField' (Shortcut:as) (ShortcutEvents es _) envi ff = do
   msfs <- mapM (\e-> findField' as e envi ff) es
   return $ headMay $ catMaybes msfs  -- returning the first field that matches

findField' fa _ _ _ = error $ "findField: wrong field address: " ++ (show fa)

-- | removes one element of signal address for all signal occurences
filterPath :: SignalAddressElem -> [SignalOccurence] -> [SignalOccurence]
filterPath fa envi = mapMaybe f envi where
   f (SignalOccurence sd (fa':fas)) | fa == fa' = Just $ SignalOccurence sd fas
   f fr = Just fr


