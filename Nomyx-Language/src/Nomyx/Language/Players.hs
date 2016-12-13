{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | All the building blocks to allow rules to manage players.
-- for example, you can change the name of player 1 with:
-- do
--    void $ modifyPlayerName 1 ("King " ++)

module Nomyx.Language.Players (
   PlayerNumber,
   PlayerName,
   PlayerInfo(..),
   Player(..),
   playerEvent,
   playerNumber, playerName,
   getPlayers, getPlayer, getPlayerName, getPlayerName',
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

import Nomyx.Language.Types
import Imprevu.Events
import Imprevu.Variables
import Nomyx.Language.Rules
import Data.Typeable
import Data.List
import Control.Lens
import Control.Arrow
import Control.Monad

-- * Players

-- | get all the players
getPlayers :: Nomex [PlayerInfo]
getPlayers = GetPlayers

-- | Get a specific player
getPlayer :: PlayerNumber -> Nomex (Maybe PlayerInfo)
getPlayer pn = do
   pls <- GetPlayers
   return $ find (\a -> a ^. playerNumber == pn) pls

-- | Get the name of a player
getPlayerName :: PlayerNumber -> Nomex (Maybe PlayerName)
getPlayerName pn = do
  p <- getPlayer pn
  return $ _playerName <$> p

-- | Get the name of a player, his number if not found
getPlayerName' :: PlayerNumber -> Nomex PlayerName
getPlayerName' pn = do
  mp <- getPlayer pn
  return $ case mp of
     Just p -> _playerName p
     Nothing -> "Player " ++ (show pn)

-- | Set the name of a player
setPlayerName :: PlayerNumber -> PlayerName -> Nomex Bool
setPlayerName = SetPlayerName

modifyPlayerName :: PlayerNumber -> (PlayerName -> PlayerName) -> Nomex Bool
modifyPlayerName pn f = do
   mn <- getPlayerName pn
   case mn of
      Just name -> setPlayerName pn (f name)
      Nothing -> return False


-- | Get the total number of players
getPlayersNumber :: Nomex Int
getPlayersNumber = length <$> getPlayers

-- | Get all the players number
getAllPlayerNumbers :: Nomex [PlayerNumber]
getAllPlayerNumbers = map _playerNumber <$> getPlayers

-- | Remove the player from the game (kick)
delPlayer :: PlayerNumber -> Nomex Bool
delPlayer = DelPlayer


-- | perform an action for each current players, new players and leaving players
-- returns the event numbers for arriving players and leaving players
forEachPlayer :: (PlayerNumber -> Nomex ()) -> (PlayerNumber -> Nomex ()) -> (PlayerNumber -> Nomex ()) -> Nomex (EventNumber, EventNumber)
forEachPlayer action actionWhenArrive actionWhenLeave = do
    pns <- getAllPlayerNumbers
    mapM_ action pns
    an <- onEvent_ (playerEvent Arrive) $ actionWhenArrive . _playerNumber
    ln <- onEvent_ (playerEvent Leave)  $ actionWhenLeave  . _playerNumber
    return (an, ln)

-- | perform the same action for each players, including new players
-- returns the event numbers for arriving players and leaving players
forEachPlayer_ :: (PlayerNumber -> Nomex ()) -> Nomex (EventNumber, EventNumber)
forEachPlayer_ action = forEachPlayer action action (\_ -> return ())

-- | create a value initialized for each players
--manages players joining and leaving
createValueForEachPlayer :: forall a. (Typeable a, Show a, Eq a) => a -> V [(PlayerNumber, a)] -> Nomex (EventNumber, EventNumber)
createValueForEachPlayer initialValue (V mv) = do
    pns <- getAllPlayerNumbers
    v <- newVar_  mv $ map (,initialValue::a) pns
    forEachPlayer (const $ return ())
                  (\p -> void $ modifyVar v ((p, initialValue) : ))
                  (\p -> void $ modifyVar v $ filter $ (/= p) . fst)

-- | create a value initialized for each players initialized to zero
--manages players joining and leaving
createValueForEachPlayer_ :: V [(PlayerNumber, Int)] -> Nomex (EventNumber, EventNumber)
createValueForEachPlayer_ = createValueForEachPlayer 0

getValueOfPlayer :: forall a. (Typeable a, Show a, Eq a) => PlayerNumber -> V [(PlayerNumber, a)] -> Nomex (Maybe a)
getValueOfPlayer pn var = do
   mvalue <- readVar var
   return $ do
      value <- mvalue
      lookup pn value

modifyValueOfPlayer :: (Eq a, Show a, Typeable a) => PlayerNumber -> V [(PlayerNumber, a)] -> (a -> a) -> Nomex Bool
modifyValueOfPlayer pn var f = modifyVar var $ map (\(a,b) -> if a == pn then (a, f b) else (a,b))

modifyAllValues :: (Eq a, Show a, Typeable a) => V [(PlayerNumber, a)] -> (a -> a) -> Nomex ()
modifyAllValues var f = void $ modifyVar var $ map $ second f

-- | show a player name based on his number
showPlayer :: PlayerNumber -> Nomex String
showPlayer pn = do
   mn <- getPlayerName pn
   case mn of
      Just name -> return name
      Nothing -> return ("Player " ++ show pn)


-- | set victory to a list of players
setVictory :: Nomex [PlayerNumber] -> Nomex ()
setVictory = SetVictory

-- | give victory to one player
giveVictory :: PlayerNumber -> Nomex ()
giveVictory pn = SetVictory $ return [pn]

-- | get the player number of the proposer of the rule
getProposerNumber :: Nomex PlayerNumber
getProposerNumber = _rProposedBy <$> getSelfRule

getProposerNumber_ :: Nomex PlayerNumber
getProposerNumber_ = getProposerNumber

-- | Build a event firing when a player arrives or leaves
playerEvent :: Player -> Event PlayerInfo
playerEvent p = SignalEvent $ Signal p
