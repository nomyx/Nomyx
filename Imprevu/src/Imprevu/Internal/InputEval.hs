{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Evaluation of the events
module Imprevu.Internal.InputEval where

import           Control.Applicative
import           Control.Category            hiding (id)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class   (MonadError (..))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Either
import qualified Data.Foldable               as F hiding (find)
import           Data.Function               (on)
import           Data.List
import           Data.Maybe
import           Data.Data
import           Data.Todo
import           Data.Typeable
import           Imprevu.Events
import           Imprevu.Internal.Event
import           Imprevu.Internal.EventEval
import           Imprevu.Internal.EvalUtils
import           Prelude                     hiding (log, (.))
import           Safe
import           GHC.Generics
import Unsafe.Coerce
import           Debug.Trace.Helpers    (traceM)


-- a form field with its title
data FormField = RadioField    String [(Int, String)]
               | TextField     String
               | TextAreaField String
               | ButtonField   String
               | CheckboxField String [(Int, String)]
                 deriving (Show, Read, Ord, Eq, Generic)

-- data sent back by the form fields
data InputData = RadioData    Int
               | CheckboxData [Int]
               | TextData     String
               | TextAreaData String
               | ButtonData
                 deriving (Show, Read, Eq, Ord)

-- | Input forms as programmed by the user
data InputForm a where
   Text     ::                                    InputForm String
   TextArea ::                                    InputForm String
   Button   ::                                    InputForm ()
   Radio    :: (Show a, Eq a, Data a) => [(a, String)] -> InputForm a
   Checkbox :: (Show a, Eq a, Data [a]) => [(a, String)] -> InputForm [a]
   deriving Typeable

deriving instance Show (InputForm a)
deriving instance Eq (InputForm e)

data InputRadio a = InputRadio String [(a, String)] deriving (Eq, Typeable, Show)

data InputRadioView = InputRadioView String [(Int, String)] deriving (Eq, Typeable, Show)

instance (Typeable a, Data a, Show a, Eq a) => Signal (InputRadio a) where
  type SignalDataType (InputRadio a) = a
  type SignalView (InputRadio a) = InputRadioView
  viewSignal (InputRadio s cs) = InputRadioView s (zip [0..] (snd <$> cs))

-- * Input triggers

-- trigger the input form with the input data
triggerInput :: (HasEvents n s) => FormField -> InputData -> SignalAddress -> EventNumber -> Evaluate n s ()
triggerInput ff i sa en = do
   evs <- use events
   let mei = find (\a -> a ^. eventNumber == en) evs
   F.forM_ mei (triggerInputSignal i sa ff)

-- trigger the input signal with the input data
triggerInputSignal :: (HasEvents n s) => InputData -> SignalAddress -> FormField -> EventInfo n -> Evaluate n s ()
triggerInputSignal ida sa ff ei@(EventInfo _ _ _ SActive _) = do
   mss <- findField ff sa ei
   case mss of
      Just ss -> triggerInputSignal' ida ss sa ei
      Nothing -> error $ "Input not found, InputData=" ++ (show ida) ++ " SignalAddress=" ++ (show sa) ++ " FormField=" ++ (show ff)
triggerInputSignal _ _ _ _ = return ()

-- trigger the input signal with the input data
triggerInputSignal' :: (HasEvents n s) => InputData -> SomeSignal -> SignalAddress -> EventInfo n -> Evaluate n s ()
--triggerInputSignal' (TextData s) (SomeSignal e) sa ei = case (cast e) of
--                                                          Just (e' :: Input String) -> triggerEvent' (SignalData e' s)                     (Just sa) [ei]
--triggerInputSignal' (TextData s) (SomeSignal e) sa ei = triggerEvent' (SignalData e s)                     (Just sa) [ei]
--triggerInputSignal' (TextAreaData s)   (TextArea)     sa ei = triggerEvent' (SignalData e s)                     (Just sa) [ei]
--triggerInputSignal' (ButtonData)       (Button)       sa ei = triggerEvent' (SignalData e ())                    (Just sa) [ei]
triggerInputSignal' (RadioData i)      (SomeSignal e)     sa ei = case (cast e) of
    Just (InputRadio _ cs) -> triggerEvent' (SignalData e (fst $ cs!!i))         (Just sa) [ei]
--triggerInputSignal' (CheckboxData is)  (Checkbox cs)  sa ei = triggerEvent' (SignalData e (fst <$> cs `sel` is)) (Just sa) [ei]
triggerInputSignal' _ _ _ _ = return ()


-- | Get the form field at a certain address
findField :: FormField -> SignalAddress -> EventInfo n -> Evaluate n s (Maybe SomeSignal)
findField ff addr (EventInfo _ e _ _ envi) = findField' addr e envi ff

findField' :: SignalAddress -> Event e -> [SignalOccurence] -> FormField -> Evaluate n s (Maybe SomeSignal)
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
      Done e2 -> findField' as (f e2) (filterPath BindR envi) ff
      Todo _  -> return Nothing
findField' (Shortcut:as) (ShortcutEvents es _) envi ff = do
   msfs <- mapM (\e-> findField' as e envi ff) es
   return $ headMay $ catMaybes msfs  -- returning the first field that matches

findField' fa _ _ _ = error $ "findField: wrong field address: " ++ (show fa)

-- | removes one element of signal address for all signal occurences
filterPath :: SignalAddressElem -> [SignalOccurence] -> [SignalOccurence]
filterPath fa envi = mapMaybe f envi where
   f (SignalOccurence sd (fa':fas)) | fa == fa' = Just $ SignalOccurence sd fas
   f fr = Just fr

-- result data from a signal
--data SignalData = forall a. (Signal a, Show a) =>
--   SignalData {signal     :: a,
--               signalData :: SignalDataType a}

getFormField :: Signal s => s -> Maybe FormField
getFormField s = case (cast s) of
                   Just (InputRadio _ choices ) -> const (Just $ RadioField    "" (zip [0..] (snd <$> choices))) (SignalData s (fst $ head choices))


                                --if (toConstr a) == (toConstr ((Input "" Button) :: Input ()))
--    then getFormField' ((unsafeCoerce a) ::Input ())
--    --  else Nothing
getFormField _ = Nothing

--getFormField' :: Input a -> Maybe FormField
--getFormField' (Input _ (Radio choices)) = Just $ RadioField    "" (zip [0..] (snd <$> choices))
--getFormField' (Input _ Text)          = Just $ TextField     ""
--getFormField' (Input _ TextArea)      = Just $ TextAreaField ""
--getFormField' (Input _ Button)        = Just $ ButtonField   ""
--getFormField' (Input _ (Checkbox choices)) = Just $ CheckboxField "" (zip [1..] (snd <$> choices))
--getFormField' _ = Nothing



sel :: [a]   -- ^ List of indices to select
    -> [Int] -- ^ List of elements
    -> [a]   -- ^ List composed of elements selected from original set by indices provided
sel xs is = map (\i -> xs!!i) is

