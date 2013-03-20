-----------------------------------------------------------------------------
--
-- Module      :  Utils
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
    
module Utils where

import Data.Maybe
import Data.Char
import Control.Monad.State
import Types
import Language.Nomyx
import Data.List
import Control.Applicative
import Control.Exception
import Data.Time
import Debug.Trace.Helpers
import Data.Lens
         
-- | this function will return just a if it can cast it to an a.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads



-- | Replaces all instances of a value in a list by another value.
replace :: Eq a => a   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)



yes = ["o", "oui", "y", "yes", "v", "vrai", "true"]
toLowerS = map toLower
isYes a = toLowerS a `elem` yes

-- | generic function to say things on transformers like GameState, ServerState etc.
say :: String -> StateT a IO ()
say = lift . putStrLn

liftT :: Show s => State s a -> StateT s IO a
liftT st = do
    s1 <- get
    let (a, s) = runState st s1
    put s
    return a


findPlayer :: PlayerName -> State Multi (Maybe PlayerMulti)
findPlayer name = find ((==) name . getL mPlayerName) <$> access mPlayers

findPlayer' :: PlayerNumber -> State Multi (Maybe PlayerMulti)
findPlayer' pn = find ((==) pn . getL mPlayerNumber) <$> access mPlayers

nomyxURL :: Network -> String
nomyxURL (Network host port) = "http://" ++ host ++ ":" ++ (show port)

getPlayersName :: PlayerNumber -> Multi -> PlayerName
getPlayersName pn multi = do
   case find (\(PlayerMulti n _ _ _ _ _) -> n==pn) (mPlayers ^$ multi) of
      Nothing -> error "getPlayersName: No player by that number"
      Just pm -> mPlayerName ^$ pm

getPlayersName' :: Game -> PlayerNumber -> PlayerName
getPlayersName' g pn = do
   case find ((==pn) . getL playerNumber) (_players g) of
      Nothing -> error "getPlayersName: No player by that number in that game"
      Just pm -> _playerName pm

-- | returns the game the player is in
getPlayersGame :: PlayerNumber -> Multi -> Maybe Game
getPlayersGame pn multi = do
        pi <- find ((==pn) . getL mPlayerNumber) (mPlayers ^$ multi)
        gn <- _inGame pi
        find ((== gn) . getL gameName) (_games multi)

getPlayersNameMay :: Game -> PlayerNumber -> Maybe PlayerName
getPlayersNameMay g pn = do
   case find ((==pn) . getL playerNumber) (_players g) of
      Nothing -> Nothing
      Just pm -> Just $ _playerName pm

commandExceptionHandler :: Maybe PlayerNumber -> Multi -> ErrorCall -> IO Multi
commandExceptionHandler mpn m e = do
   putStrLn $ "Exception in rule: " ++ (show e)
   case mpn of
      Just pn -> do
         let g = fromJust $ getPlayersGame pn m
         let g' = execState (Utils.output ("Error in command: " ++ (show e)) pn) g
         return $ execState (modifyGame g') m
      Nothing -> return m


-- | finds the corresponding game in the multistate and replaces it.
modifyGame :: Game -> State Multi ()
modifyGame g = do
   gs <- access games
   case find ((== _gameName g) . getL gameName) gs of
      Nothing -> error "modifyGame: No game by that name"
      Just oldg -> do
         let newgs = replace oldg g gs
         games ~= newgs
         return ()


output :: String -> PlayerNumber -> State Game ()
output s pn = void $ outputs %= ((pn, s) : )

outputAll :: String -> State Game ()
outputAll s = access players >>= mapM_ ((Utils.output s) . _playerNumber)


execWithMulti :: UTCTime -> StateT Multi IO () -> Multi -> IO Multi
execWithMulti t ms m = do
   let setTime g = g {_currentTime = t}
   let m' = games `modL` (map setTime) $ m
   execStateT ms m'

tracePN :: (Monad m ) => PlayerNumber -> String -> m ()
tracePN pn s = traceM $ "Player " ++ (show pn) ++ " " ++ s



