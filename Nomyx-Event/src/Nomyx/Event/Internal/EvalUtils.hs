{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | additional tools for evaluation
module Nomyx.Event.Internal.EvalUtils where

import           Control.Applicative
import           Control.Category
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Data.Typeable
import           Nomyx.Event.Events
import           Nomyx.Event.Internal.Utils
import           Prelude                   hiding (log, (.))
import           Safe

-- find a signal occurence in an environment
lookupSignal :: Typeable a => Signal a -> SignalAddress -> [SignalOccurence] -> Maybe a
lookupSignal s sa envi = headMay $ mapMaybe (getSignalData s sa) envi

--get the signal data from the signal occurence
getSignalData :: Typeable a => Signal a -> SignalAddress -> SignalOccurence -> Maybe a
getSignalData s sa (SignalOccurence (SignalData s' res) sa') = do
   ((s'', res') :: (Signal a, a)) <- cast (s', res)
   if (s'' == s) && (sa' == sa) then Just res' else Nothing


--errorHandler :: EventNumber -> String -> Evaluate ()
--errorHandler en s = undefined --do
--   rn <- use eRuleNumber
--   logAll $ "Error (triggered by event " ++ show en ++ "): " ++ s

--logPlayer :: PlayerNumber -> String -> Evaluate ()
--logPlayer pn = log (Just pn)

--logAll :: String -> Evaluate ()
--logAll = log Nothing

--log :: Maybe PlayerNumber -> String -> Evaluate ()
--log mpn s = focusGame $ do
--   time <- undefined --use currentTime
--`   void $ logs %= (Log mpn time s : )



--evalRuleActive :: Evaluate Bool
--evalRuleActive = do
--   rn <- use eRuleNumber
--   rs <- use (eGame . rules)
--   return $ (rn == 0) ||
--      case find (\r -> _rNumber r == rn) rs of
--         Just r -> _rStatus r == Active
--         Nothing -> True --TODO why should there be an evaluating rule not in the list?
--
--
----replace temporarily the rule number used for evaluation
--
--withRN :: RuleNumber -> Evaluate a -> Evaluate a
--withRN rn eval = do
--   oldRn <- gets _eRuleNumber
--   eRuleNumber .= rn
--   a <- eval
--   eRuleNumber .= oldRn
--   return a

instance Eq (Signal n) where
  (Signal e1) == (Signal e2) = e1 === e2

instance Eq SomeSignal where
  (SomeSignal e1) == (SomeSignal e2) = e1 === e2

instance Show (EventInfo n) where
   show (EventInfo en _ _ s envi) =
      "event num: " ++ (show en) ++
   --   ", rule num: " ++ (show rn) ++
      ", envs: " ++ (show envi) ++
      ", status: " ++ (show s)