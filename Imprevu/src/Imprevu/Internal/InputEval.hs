{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE DeriveGeneric        #-}

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
import           Data.Todo
import           Data.Typeable
import           Imprevu.Events
import           Imprevu.Internal.Event
import           Imprevu.Internal.EventEval
import           Imprevu.Internal.EvalUtils
import           Prelude                     hiding (log, (.))
import           Safe
import           GHC.Generics


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

-- * Input triggers

-- trigger the input form with the input data
triggerInput :: (HasEvents n s) => FormField -> InputData -> SignalAddress -> EventNumber -> Evaluate n s ()
triggerInput ff i sa en = do
   evs <- use events
   let mei = find (\a -> a ^. eventNumber == en) evs
   F.forM_ mei (triggerInputSignal i sa ff)

-- trigger the input signal with the input data
triggerInputSignal :: InputData -> SignalAddress -> FormField -> EventInfo n -> Evaluate n s ()
triggerInputSignal ide sa ff ei@(EventInfo _ _ _ SActive _) = do
   i <- findField ff sa ei
   case i of
      Just sf -> triggerInputSignal' ide sf sa ei
      Nothing -> error $ "Input not found, InputData=" ++ (show ide) ++ " SignalAddress=" ++ (show sa) ++ " FormField=" ++ (show ff)
triggerInputSignal _ _ _ _ = return ()

-- trigger the input signal with the input data
triggerInputSignal' :: InputData -> SomeSignal -> SignalAddress -> EventInfo n -> Evaluate n s ()
--triggerInputSignal' (TextData s)      (SomeSignal e)         sa ei = triggerEvent' (SignalData e s)                     (Just sa) [ei]
--triggerInputSignal' (TextAreaData s)  (SomeSignal e@(Input _  TextArea))     sa ei = triggerEvent' (SignalData e s)                     (Just sa) [ei]
--triggerInputSignal' (ButtonData)      (SomeSignal e@(Input _  Button))       sa ei = triggerEvent' (SignalData e ())                    (Just sa) [ei]
--triggerInputSignal' (RadioData i)     (SomeSignal e@(Input _ (Radio cs)))    sa ei = triggerEvent' (SignalData e (fst $ cs!!i))         (Just sa) [ei]
--triggerInputSignal' (CheckboxData is) (SomeSignal e@(Input _ (Checkbox cs))) sa ei = triggerEvent' (SignalData e (fst <$> cs `sel` is)) (Just sa) [ei]
triggerInputSignal' _ _ _ _ = return ()


-- | Get the form field at a certain address
findField :: FormField -> SignalAddress -> EventInfo n -> Evaluate n s (Maybe SomeSignal)
findField ff addr (EventInfo _ e _ _ envi) = findField' addr e envi ff

findField' :: SignalAddress -> Event e -> [SignalOccurence] -> FormField -> Evaluate n s (Maybe SomeSignal)
findField' []         (SignalEvent f)    _   ff = return $ do
   ff' <- getFormField (SomeSignal f)
   guard (ff' == ff)
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

getFormField :: SomeSignal -> Maybe FormField
--getFormField (SomeSignal (Signal a)) = case cast a of
--   Just (Radio choices :: InputForm a) -> Just $ RadioField    "" (zip [0..] (snd <$> choices))
--   Just (Text :: InputForm String)          -> Just $ TextField     ""
--   Just (TextArea)      -> Just $ TextAreaField ""
--   Just (Button)        -> Just $ ButtonField   ""
--   Just (Checkbox choices) -> Just $ CheckboxField "" (zip [1..] (snd <$> choices))
getFormField _ = Nothing
