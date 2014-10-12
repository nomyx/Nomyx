{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | This file gives a list of example rules that the players can submit.
--You can copy-paste them in the field "Code" of the web GUI.
--Don't hesitate to get inspiration from there and create your own rules!
module Language.Nomyx.Examples(
   nothing,
   helloWorld,
   accounts,
   createBankAccount,
   winXEcuPerDay,
   winXEcuOnRuleAccepted,
   moneyTransfer,
   delRule,
   voteWithMajority,
   king,
   makeKing,
   monarchy,
   revolution,
   displayCurrentTime,
   displayActivateTime,
   iWin,
   returnToDemocracy,
   victoryXRules,
   victoryXEcu,
   noGroupVictory,
   banPlayer,
--   referendum,
--   referendumOnKickPlayer,
--   gameMasterElections,
   gameMaster,
   bravoButton,
   enterHaiku,
   displayBankAccount,
   helloButton,
   module X) where

import Data.Function
import Data.Time.Clock as X hiding (getCurrentTime)
import Data.Time.Recurrence as X hiding (filter)
import Data.List as X
import Data.Typeable
import Data.Maybe
import Control.Arrow
import Control.Monad as X
import Safe (readDef)
import Language.Nomyx

-- | A rule that does nothing
nothing :: Rule
nothing = return ()

-- | A rule that says hello to all players
helloWorld :: Rule
helloWorld = outputAll_ "hello, world!"

-- | account variable name and type
accounts :: MsgVar [(PlayerNumber, Int)]
accounts = msgVar "Accounts"

-- | Create a bank account for each players
createBankAccount :: Rule
createBankAccount = void $ createValueForEachPlayer_ accounts

-- | Permanently display the bank accounts
displayBankAccount :: Rule
displayBankAccount = do
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
          pls <- liftNomexNE getAllPlayerNumbers
          guard (length pls >= 2) >> do
             dst <- inputRadio' src "Transfer money to player: " (delete src $ sort pls)
             amount <- inputText src ("Select Amount to transfert to player " ++ show dst ++ ": ")
             return (dst, readDef 0 amount)
   void $ forEachPlayer_ (\pn -> void $ onEvent_ (askAmount pn) (transfer pn))

-- | helper function to transfer money from first player to second player
transfer :: PlayerNumber -> (PlayerNumber, Int) -> Nomex ()
transfer src (dst, amount) = do
   balance <- liftEffect $ getValueOfPlayer src accounts
   if (amount > 0 && fromJust balance >= amount) then do
      modifyValueOfPlayer dst accounts (\a -> a + amount)
      modifyValueOfPlayer src accounts (\a -> a - amount)
      void $ newOutput_ (Just src) ("You gave " ++ (show amount) ++ " ecu(s) to player " ++ show dst)
      void $ newOutput_ (Just dst) ("Player " ++ show src ++ " gave you " ++ (show amount) ++ " ecu(s)")
   else void $ newOutput_ (Just src) ("Insufficient balance or wrong amount")

-- | delete a rule
delRule :: RuleNumber -> Rule
delRule rn = suppressRule_ rn >> autoDelete

-- | player pn is the king: we create a variable King to identify him,
-- and we prefix his name with "King"
makeKing :: PlayerNumber -> Rule
makeKing pn = do
   newMsgVar_ "King" pn
   void $ modifyPlayerName pn ("King " ++)

king :: MsgVar PlayerNumber
king = msgVar "King"

-- | Monarchy: only the king decides which rules to accept or reject
monarchy :: PlayerNumber -> Rule
monarchy pn = do
   makeKing pn
   void $ onEvent_ (ruleEvent Proposed) $ \rule -> do
      k <- readMsgVar_ king
      void $ onInputRadioOnce ("Your Royal Highness, do you accept rule " ++ (show $ _rNumber rule) ++ "?") [True, False] (activateOrRejectRule rule) k

-- | Revolution! Hail to the king!
-- This rule suppresses the democracy (usually rules 1 and 2), installs the king and activates monarchy.
revolution :: PlayerNumber -> Rule
revolution pn = do
    suppressRule 1
    rNum <- addRule' "Monarchy" (monarchy pn) ("monarchy " ++ (show pn)) "Monarchy: only the king can vote on new rules"
    activateRule_ rNum
    autoDelete

-- | set the victory for players having more than X accepted rules
victoryXRules :: Int -> Rule
victoryXRules x = setVictory $ do
    rs <- getRules
    let counts :: [(PlayerNumber,Int)]
        counts = map (_rProposedBy . head &&& length) $ groupBy ((==) `on` _rProposedBy) rs
    let victorious = map fst $ filter ((>= x) . snd) counts
    return victorious

victoryXEcu :: Int -> Rule
victoryXEcu x = setVictory $ do
    as <- readMsgVar accounts
    let victorious as = map fst $ filter ((>= x) . snd) as
    return $ maybe [] victorious as

-- | will display the current time (when refreshing the screen)
displayCurrentTime :: Rule
displayCurrentTime = void $ outputAll $ do
    t <- getCurrentTime
    return $ "The current time is: " ++ (show t)

-- | will display the time at which the rule as been activated
displayActivateTime :: Nomex ()
displayActivateTime = do
   time <- liftEffect getCurrentTime
   outputAll_ $ "This rule was activated at: " ++ (show time)

-- | Only one player can achieve victory: No group victory.
-- Forbidding group victory usually becomes necessary when lowering the voting quorum:
-- a coalition of players could simply force a "victory" rule and win the game.
noGroupVictory ::  Rule
noGroupVictory = do
   let testVictory (VictoryInfo _ cond) = do
       vics <- liftEffect cond
       when (length vics >1) $ setVictory (return []) --unset victory condition
   void $ onEvent_ victoryEvent testVictory

-- | Rule that state that you win. Good luck on having this accepted by other players ;)
iWin :: Rule
iWin = liftEffect getProposerNumber >>= giveVictory


-- | a majority vote, with the folowing parameters:
-- a quorum of 2 voters is necessary for the validity of the vote
-- the vote is assessed after every vote in case the winner is already known
-- the vote will finish anyway after one day
voteWithMajority :: Rule
voteWithMajority = onRuleProposed $ callVoteRule (majority `withQuorum` 2) oneDay

-- | Change current system (the rules passed in parameter) to absolute majority (half participants plus one)
returnToDemocracy :: [RuleNumber] -> Rule
returnToDemocracy rs = do
   mapM_ suppressRule rs
   rNum <- addRule' "vote with majority" voteWithMajority "voteWithMajority" "majority with a quorum of 2"
   activateRule_ rNum
   autoDelete

-- | kick a player and prevent him from returning
banPlayer :: PlayerNumber -> Rule
banPlayer pn = do
   delPlayer pn
   void $ onEvent_ (playerEvent Arrive) $ const $ void $ delPlayer pn


-- | display a button and greets you when pressed (for player 1)
bravoButton :: Rule
bravoButton = void $ onInputButton_ "Click here:" (const $ outputAll_ "Bravo!") 1

-- | display a button to greet other players
helloButton :: Rule
helloButton = do
   --get your own player number
   me <- getProposerNumber_
   --create an output for me only
   let displayMsg a = void $ newOutput_ Nothing ("Msg: " ++ a)
   --create a button for me, which will display the output when clicked
   let button = do
       all <- liftNomexNE getPlayers
       guard (length all >= 2) >> inputText me "send a message"
   void $ onEvent_ button displayMsg

enterHaiku :: Rule
enterHaiku = void $ onInputTextarea_ "Enter a haiku:" outputAll_ 1


-- * Referendum & elections

-- | triggers a referendum, if the outcome is yes player 2 will be kicked
--referendumOnKickPlayer :: Rule
--referendumOnKickPlayer = referendum " kick player 2" (void $ delPlayer 2)
--
---- | triggers elections (all players are candidates), the winner becomes game master
--gameMasterElections :: Rule
--gameMasterElections = do
--   pls <- liftEffect getPlayers
--   elections "Game Master" pls makeGM

makeGM :: PlayerNumber -> Nomex ()
makeGM pn = do
   newMsgVar "GameMaster" pn
   void $ modifyPlayerName pn ("GameMaster " ++)

gameMaster :: MsgVar PlayerNumber
gameMaster = msgVar "GameMaster"

tournamentMasterCandidates :: Rule
tournamentMasterCandidates = do
   let tournamentMasterCandidates = msgVar "tournamentMasterCandidates" :: MsgVar [PlayerNumber]
   let candidate pn = void $ modifyMsgVar tournamentMasterCandidates (pn : )
   let displayCandidates pns = return $ "Candidates for the election of Tournament Master: Players #" ++ intercalate ", " (map show pns)
   newMsgVar_ (getMsgVarName tournamentMasterCandidates) ([] :: [PlayerNumber])
   forEachPlayer_ (\pn -> void $ onInputButtonOnce "I am candidate for the next Tournament Master elections " (const $ candidate pn) pn)
   void $ displayVar' Nothing tournamentMasterCandidates displayCandidates

-- | castle structure
data Castle = Castle { towers :: Int, dungeon :: Bool }
              deriving (Typeable, Show, Eq)

castles :: MsgVar [(PlayerNumber, Castle)]
castles = msgVar "Castles"

--castleVictory :: RuleFunc
--castleVictory = ruleFunc $ do
--  let checkVict cs = do
--       let vict = map fst $ filter ((== (Castle 4 True)) . snd) cs
--       when (length vict > 0) $ setVictory vict
--  onMsgVarEvent castles $ (\(VUpdated cs) -> checkVict cs)

