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


class (Signal v) => SignalView v where
  type SignalViewed v :: * -> *
  -- construct a view
  viewSignal :: SignalViewed v a -> v
    --
  actSignal :: SignalViewed v a -> SignalDataType v -> SignalDataType (SignalViewed v a)

data InputRadio a = InputRadio String [(a, String)] deriving (Eq, Typeable, Show)

data InputRadioView = InputRadioView String [(Int, String)] deriving (Eq, Typeable, Show)

instance (Eq a, Typeable a, Show a) => Signal(InputRadio a) where
  type SignalDataType (InputRadio a) = a

instance Signal(InputRadioView) where
  type SignalDataType (InputRadioView) = Int

instance SignalView InputRadioView where
  type SignalViewed InputRadioView = InputRadio

  viewSignal :: InputRadio a -> InputRadioView
  viewSignal (InputRadio s cs) = InputRadioView s (zip [0..] (snd <$> cs))
  -- fire a view
  actSignal :: InputRadio a -> Int -> a
  actSignal (InputRadio _ cs) i = fst $ cs!!i

-- * Input triggers
--data Proxy a

-- trigger the input form with the input data
triggerInput :: (HasEvents n s, SignalView v) => v -> SignalDataType v -> SignalAddress -> EventNumber -> Evaluate n s ()
triggerInput sv dv sa en = do
   evs <- use events
   let mei = find (\a -> a ^. eventNumber == en) evs
   mapM_ (triggerInputSignal sv dv sa) mei

-- trigger the input signal with the input data
triggerInputSignal :: forall n s v. (HasEvents n s, SignalView v) => v -> SignalDataType v -> SignalAddress -> EventInfo n -> Evaluate n s ()
triggerInputSignal sv dv sa ei@(EventInfo _ _ _ SActive _) = do
       mss <- findField sv sa ei
       case mss of
          Just (SomeSignal e) -> case (cast (e, sv, dv)) of
                                  Just (e'@(InputRadio _ cs), (InputRadioView _ _), dv') -> triggerEvent' (SignalData e (fst $ cs!!dv')) (Just sa) [ei]

--            let dat :: SignalDataType (SignalViewed v a)
--                dat = actSignal e' dv
--            let sigDat :: SignalData
--                sigDat = SignalData e' dat
--            triggerEvent' undefined  (Just sa) [ei]
          Nothing -> error $ "Input not found, signal view=" ++ (show sv) ++ " SignalAddress=" ++ (show sa) ++ " signal view data=" ++ (show dv)
triggerInputSignal _ _ _ _ = return ()


-- | Get the form field at a certain address
findField :: SignalView v => v -> SignalAddress -> EventInfo n -> Evaluate n s (Maybe SomeSignal)
findField sv sa (EventInfo _ e _ _ envi) = findField' sa e envi sv

findField' :: SignalView v => SignalAddress -> Event e -> [SignalOccurence] -> v -> Evaluate n s (Maybe SomeSignal)
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
findField' (Shortcut:as) (ShortcutEvents es _) envi ff = undefined --do
--   msfs <- mapM (\e-> findField' as e envi ff) es
--   return $ headMay $ catMaybes msfs  -- returning the first field that matches

findField' fa _ _ _ = error $ "findField: wrong field address: " ++ (show fa)

-- | removes one element of signal address for all signal occurences
filterPath :: SignalAddressElem -> [SignalOccurence] -> [SignalOccurence]
filterPath fa envi = mapMaybe f envi where
   f (SignalOccurence sd (fa':fas)) | fa == fa' = Just $ SignalOccurence sd fas
   f fr = Just fr

sel :: [a]   -- ^ List of indices to select
    -> [Int] -- ^ List of elements
    -> [a]   -- ^ List composed of elements selected from original set by indices provided
sel xs is = map (\i -> xs!!i) is


--triggerInputSignal' (TextData s) (SomeSignal e) sa ei = case (cast e) of
--                                                          Just (e' :: Input String) -> triggerEvent' (SignalData e' s)                     (Just sa) [ei]
--triggerInputSignal' (TextData s) (SomeSignal e) sa ei = triggerEvent' (SignalData e s)                     (Just sa) [ei]
--triggerInputSignal' (TextAreaData s)   (TextArea)     sa ei = triggerEvent' (SignalData e s)                     (Just sa) [ei]
--triggerInputSignal' (ButtonData)       (Button)       sa ei = triggerEvent' (SignalData e ())                    (Just sa) [ei]
--triggerInputSignal' (RadioData i)      (SomeSignal e)     sa ei = case (cast e) of
--    Just (InputRadio _ cs) -> triggerEvent' (SignalData e (fst $ cs!!i))         (Just sa) [ei]
--triggerInputSignal' (CheckboxData is)  (Checkbox cs)  sa ei = triggerEvent' (SignalData e (fst <$> cs `sel` is)) (Just sa) [ei]

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

