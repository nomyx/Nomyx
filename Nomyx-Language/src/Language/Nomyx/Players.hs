{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | All the building blocks to allow rules to manage players.
-- for example, you can change the name of player 1 with:
-- do
--    void $ modifyPlayerName 1 ("King " ++)

module Language.Nomyx.Players (
   PlayerNumber,
   PlayerName,
   PlayerInfo(..),
   Player(..),
   playerNumber, playerName,
   getPlayers, getPlayer, getPlayerName,
   setPlayerName,
   modifyPlayerName,
   getPlayersNumber, getAllPlayerNumbers,
   delPlayer,
   forEachPlayer, forEachPlayer_,
   createValueForEachPlayer, createValueForEachPlayer_,
   getValueOfPlayer,
   modifyValueOfPlayer, modifyAllValues,
   showPlayer,
   getProposerNumber, getProposerNumber_,
   setVictory, 
   giveVictory
   ) where

import Language.Nomyx.Expression
import Language.Nomyx.Events
import Language.Nomyx.Variables
import Language.Nomyx.Rules
import Data.Typeable
import Data.List
import Data.Lens
import Control.Applicative
import Control.Arrow
import Control.Monad

-- * Players

-- | get all the players
getPlayers :: NomexNE [PlayerInfo]
getPlayers = GetPlayers

-- | Get a specific player
getPlayer :: PlayerNumber -> NomexNE (Maybe PlayerInfo)
getPlayer pn = do
   pls <- GetPlayers
   return $ find ((== pn) . getL playerNumber) pls

-- | Set the name of a player
getPlayerName :: PlayerNumber -> NomexNE (Maybe PlayerName)
getPlayerName pn = do
  p <- getPlayer pn
  return $ _playerName <$> p

-- | Set the name of a player
setPlayerName :: PlayerNumber -> PlayerName -> Nomex Bool
setPlayerName = SetPlayerName

modifyPlayerName :: PlayerNumber -> (PlayerName -> PlayerName) -> Nomex Bool
modifyPlayerName pn f = do
   mn <- liftEffect $ getPlayerName pn
   case mn of
      Just name -> setPlayerName pn (f name)
      Nothing -> return False


-- | Get the total number of players
getPlayersNumber :: NomexNE Int
getPlayersNumber = length <$> getPlayers

-- | Get all the players number
getAllPlayerNumbers :: NomexNE [PlayerNumber]
getAllPlayerNumbers = map _playerNumber <$> getPlayers

-- | Remove the player from the game (kick)
delPlayer :: PlayerNumber -> Nomex Bool
delPlayer = DelPlayer


-- | perform an action for each current players, new players and leaving players
-- returns the event numbers for arriving players and leaving players
forEachPlayer :: (PlayerNumber -> Nomex ()) -> (PlayerNumber -> Nomex ()) -> (PlayerNumber -> Nomex ()) -> Nomex (EventNumber, EventNumber)
forEachPlayer action actionWhenArrive actionWhenLeave = do
    pns <- liftEffect getAllPlayerNumbers
    mapM_ action pns
    an <- onEvent_ (Player Arrive) $ actionWhenArrive . _playerNumber
    ln <- onEvent_ (Player Leave)  $ actionWhenLeave  . _playerNumber
    return (an, ln)

-- | perform the same action for each players, including new players
-- returns the event numbers for arriving players and leaving players
forEachPlayer_ :: (PlayerNumber -> Nomex ()) -> Nomex (EventNumber, EventNumber)
forEachPlayer_ action = forEachPlayer action action (\_ -> return ())

-- | create a value initialized for each players
--manages players joining and leaving
createValueForEachPlayer :: forall a. (Typeable a, Show a, Eq a) => a -> MsgVar [(PlayerNumber, a)] -> Nomex (EventNumber, EventNumber)
createValueForEachPlayer initialValue mv = do
    pns <- liftEffect getAllPlayerNumbers
    v <- newMsgVar_ (getMsgVarName mv) $ map (,initialValue::a) pns
    forEachPlayer (const $ return ())
                  (\p -> void $ modifyMsgVar v ((p, initialValue) : ))
                  (\p -> void $ modifyMsgVar v $ filter $ (/= p) . fst)

-- | create a value initialized for each players initialized to zero
--manages players joining and leaving
createValueForEachPlayer_ :: MsgVar [(PlayerNumber, Int)] -> Nomex (EventNumber, EventNumber)
createValueForEachPlayer_ = createValueForEachPlayer 0

getValueOfPlayer :: forall a. (Typeable a, Show a, Eq a) => PlayerNumber -> MsgVar [(PlayerNumber, a)] -> NomexNE (Maybe a)
getValueOfPlayer pn var = do
   mvalue <- readMsgVar var
   return $ do
      value <- mvalue
      lookup pn value

modifyValueOfPlayer :: (Eq a, Show a, Typeable a) => PlayerNumber -> MsgVar [(PlayerNumber, a)] -> (a -> a) -> Nomex Bool
modifyValueOfPlayer pn var f = modifyMsgVar var $ map (\(a,b) -> if a == pn then (a, f b) else (a,b))

modifyAllValues :: (Eq a, Show a, Typeable a) => MsgVar [(PlayerNumber, a)] -> (a -> a) -> Nomex ()
modifyAllValues var f = void $ modifyMsgVar var $ map $ second f

-- | show a player name based on his number
showPlayer :: PlayerNumber -> NomexNE String
showPlayer pn = do
   mn <- getPlayerName pn
   case mn of
      Just name -> return name
      Nothing -> return ("Player " ++ show pn)


-- | set victory to a list of players
setVictory :: NomexNE [PlayerNumber] -> Nomex ()
setVictory = SetVictory

-- | give victory to one player
giveVictory :: PlayerNumber -> Nomex ()
giveVictory pn = SetVictory $ return [pn]

-- | get the player number of the proposer of the rule
getProposerNumber :: NomexNE PlayerNumber
getProposerNumber = _rProposedBy <$> getSelfRule 

getProposerNumber_ :: Nomex PlayerNumber
getProposerNumber_ = liftEffect getProposerNumber
