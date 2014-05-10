{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
   displayTime,
   iWin,
   returnToDemocracy,
   victoryXRules,
   victoryXEcu,
   --noGroupVictory,
   banPlayer,
   referendum,
   referendumOnKickPlayer,
   gameMasterElections,
   gameMaster,
   bravoButton,
   enterHaiku,
   displayBankAccount,
   module X) where

import Data.Function
import Data.Time.Clock as X hiding (getCurrentTime)
import Data.Time.Recurrence as X hiding (filter)
import Data.List as X
import Data.Typeable
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
winXEcuOnRuleAccepted x = void $ onEvent_ (RuleEv Activated) $ \rule -> void $ modifyValueOfPlayer (_rProposedBy rule) accounts (+x)

-- | a player can transfer money to another player
-- it does not accept new players or check if balance is positive, to keep the example simple
moneyTransfer :: Rule
moneyTransfer = do
   pls <- liftEffect getAllPlayerNumbers
   when (length pls >= 2) $ void $ forEachPlayer_ (selPlayer pls) where
      selPlayer pls src = void $ onInputRadio_ "Transfer money to player: " (delete src $ sort pls) (selAmount src) src
      selAmount src dst = void $ onInputTextOnce ("Select Amount to transfert to player: " ++ show dst) (transfer src dst) src
      transfer src dst amount = do
          modifyValueOfPlayer dst accounts (\a -> a + (readDef 0 amount))
          modifyValueOfPlayer src accounts (\a -> a - (readDef 0 amount))
          void $ newOutput (Just src) (return $ "You gave " ++ amount ++ " ecus to player " ++ show dst)
          void $ newOutput (Just dst) (return $ "Player " ++ show src ++ " gaved you " ++ amount ++ "ecus")

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
monarchy :: Rule
monarchy = void $ onEvent_ (RuleEv Proposed) $ \rule -> do
    k <- readMsgVar_ king
    void $ onInputRadioOnce ("Your Royal Highness, do you accept rule " ++ (show $ _rNumber rule) ++ "?") [True, False] (activateOrReject rule) k

-- | Revolution! Hail to the king!
-- This rule suppresses the democracy (usually rules 1 and 2), installs the king and activates monarchy.
revolution :: PlayerNumber -> Rule
revolution player = do
    suppressRule 1
    makeKing player
    rNum <- addRuleParams "Monarchy" monarchy "monarchy" "Monarchy: only the king can vote on new rules"
    activateRule_ rNum
    --autoDelete

-- | set the victory for players having more than X accepted rules
victoryXRules :: Int -> Rule
victoryXRules x = setVictory $ do
    rs <- getRules
    let counts = map (_rProposedBy . head &&& length) $ groupBy ((==) `on` _rProposedBy) rs
    let victorious = map fst $ filter ((>= x) . snd) counts
    return victorious

victoryXEcu :: Int -> Rule
victoryXEcu x = setVictory $ do
    as <- readMsgVar accounts
    let victorious as = map fst $ filter ((>= x) . snd) as
    return $ maybe [] victorious as

-- | will display the time to all players in 5 seconds
displayTime :: Rule
displayTime = void $ outputAll $ do
    t <- getCurrentTime
    return $ show t

-- | Only one player can achieve victory: No group victory.
-- Forbidding group victory usually becomes necessary when lowering the voting quorum:
-- a coalition of players could simply force a "victory" rule and win the game.
--noGroupVictory ::  RuleFunc
--noGroupVictory = ruleFunc $ onEvent_ Victory $ \(VictoryData ps) -> when (length ps >1) $ setVictory []

-- | Rule that state that you win. Good luck on having this accepted by other players ;)
iWin :: Rule
iWin = liftEffect getProposerNumber >>= giveVictory


-- | a majority vote, with the folowing parameters:
-- a quorum of 2 voters is necessary for the validity of the vote
-- the vote is assessed after every vote in case the winner is already known
-- the vote will finish anyway after one day
voteWithMajority :: Rule
voteWithMajority = onRuleProposed $ voteWith_ (majority `withQuorum` 2) $ assessOnEveryVote >> assessOnTimeDelay oneDay

-- | Change current system (the rules passed in parameter) to absolute majority (half participants plus one)
returnToDemocracy :: [RuleNumber] -> Rule
returnToDemocracy rs = do
   mapM_ suppressRule rs
   rNum <- addRuleParams "vote with majority" voteWithMajority "voteWithMajority" "majority with a quorum of 2"
   activateRule_ rNum
   autoDelete

-- | kick a player and prevent him from returning
banPlayer :: PlayerNumber -> Rule
banPlayer pn = do
   delPlayer pn
   void $ onEvent_ (Player Arrive) $ const $ void $ delPlayer pn

-- * Referendum & elections

-- | triggers a referendum, if the outcome is yes player 2 will be kicked
referendumOnKickPlayer :: Rule
referendumOnKickPlayer = referendum " kick player 2" (void $ delPlayer 2)

-- | triggers elections (all players are candidates), the winner becomes game master
gameMasterElections :: Rule
gameMasterElections = do
   pls <- liftEffect getPlayers
   elections "Game Master" pls makeGM

makeGM :: PlayerNumber -> Nomex ()
makeGM pn = do
   newMsgVar "GameMaster" pn
   void $ modifyPlayerName pn ("GameMaster " ++)

gameMaster :: MsgVar PlayerNumber
gameMaster = msgVar "GameMaster"

-- | display a button and greets you when pressed (for player 1)
bravoButton :: Rule
bravoButton = void $ onInputButton_ "Click here:" (const $ outputAll_ "Bravo!") 1

enterHaiku :: Rule
enterHaiku = void $ onInputTextarea_ "Enter a haiku:" outputAll_ 1

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

