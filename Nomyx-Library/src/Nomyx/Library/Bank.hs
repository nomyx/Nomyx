 {-# LANGUAGE DoAndIfThenElse      #-}

-- | This file gives a list of example rules that the players can submit.
module Nomyx.Library.Bank where

import Prelude
import Data.Time.Recurrence as X hiding (filter)
import Data.List as X
import Data.Maybe
import Control.Monad as X
import Safe (readDef)
import Nomyx.Language


-- | account variable name and type
accounts :: V [(PlayerNumber, Int)]
accounts = V "Accounts"

-- | Create a bank account for each players
createBankAccounts :: Rule
createBankAccounts = void $ createValueForEachPlayer_ accounts

-- | Declare an API to deposit money for a player
-- The return value shows if the transaction was successful.
depositAPI :: APICall (PlayerNumber, Int) Bool
depositAPI = APICall "deposit"

-- | Declare an API to withdraw money for a player.
-- The return value shows if the transaction was successful.
withdrawAPI :: APICall (PlayerNumber, Int) Bool
withdrawAPI = APICall "withdraw"

-- | Declare an API to get the balance of a player.
balanceAPI :: APICall PlayerNumber (Maybe Int)
balanceAPI = APICall "getBalance"

bankServices :: Nomex ()
bankServices = do
  void $ onAPICall depositAPI deposit
  void $ onAPICall withdrawAPI withdraw
  void $ onAPICall balanceAPI getBalance

deposit :: (PlayerNumber, Int) -> Nomex Bool
deposit (pn, amount) = do
  if amount > 0 then modifyValueOfPlayer pn accounts (+ amount)
  else return False

withdraw :: (PlayerNumber, Int) -> Nomex Bool
withdraw (pn, amount) = do
  balance <- getValueOfPlayer pn accounts
  if (amount > 0 && fromJust balance >= amount) then modifyValueOfPlayer pn accounts (\a -> a - amount)
  else return False

getBalance :: PlayerNumber -> Nomex (Maybe Int)
getBalance pn = getValueOfPlayer pn accounts

-- | Permanently display the bank accounts
displayBankAccounts :: Rule
displayBankAccounts = do
   let displayOneAccount (account_pn, a) = do
        name <- showPlayer account_pn
        return $ name ++ "\t" ++ show a ++ "\n"
   let displayAccounts l = do
        d <- concatMapM displayOneAccount l
        return $ "Accounts:\n" ++ d
   void $ displayVar' Nothing accounts displayAccounts

-- | each player wins X Ecu each day
-- you can also try with "minutly" or "monthly" instead of "daily" and everything in the "time-recurrence" package
winXEcuPerDay :: Int -> Rule
winXEcuPerDay x = schedule_ (recur daily) $ modifyAllValues accounts (+x)

-- | a player wins X Ecu if a rule proposed is accepted
winXEcuOnRuleAccepted :: Int -> Rule
winXEcuOnRuleAccepted x = void $ onEvent_ (ruleEvent Activated) $ \rule -> void $ modifyValueOfPlayer (_rProposedBy rule) accounts (+x)

-- | a player can transfer money to another player
moneyTransfer :: Rule
moneyTransfer = do
   let askAmount :: PlayerNumber -> Event (PlayerNumber, Int)
       askAmount src = do
          pls <- liftEvent getAllPlayerNumbers
          guard (length pls >= 2) >> do
             let pnames = map (\a -> (a, show a)) (delete src $ sort pls)
             dst <- inputRadio src "Transfer money to player: " pnames
             amount <- inputText src ("Select Amount to transfert to player " ++ show dst ++ ": ")
             return (dst, readDef 0 amount)
   void $ forEachPlayer_ (\pn -> void $ onEvent_ (askAmount pn) (transfer pn))

-- | helper function to transfer money from first player to second player
transfer :: PlayerNumber -> (PlayerNumber, Int) -> Nomex ()
transfer src (dst, amount) = do
   withdrawOK <- callAPIBlocking withdrawAPI (src, amount)
   if withdrawOK then do
      depositOK <- callAPIBlocking depositAPI (dst, amount)
      if depositOK then do
        void $ newOutput_ (Just src) ("You transfered " ++ (show amount) ++ " ecu(s) to player " ++ (show dst))
        void $ newOutput_ (Just dst) ("You received " ++ (show amount) ++ " ecu(s) from player " ++ (show src))
      else do
        --If transaction failed, deposit back the money
        callAPIBlocking depositAPI (src, amount)
        void $ newOutput_ (Just src) ("Transaction failed")
    else do
      void $ newOutput_ (Just src) ("Insufficient balance or wrong amount")
