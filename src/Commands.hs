
-- | This module handles intrepreting players' commands.
-- DEPRECATED
module Commands where

import Text.ParserCombinators.Parsec
import Data.List
import Utils
import Multi
import Data.Maybe
import Language.Nomic.Expression

-- | All commands issuable.
data Command = ListGame
--             | Name
             | NewGame
             | JoinGame
             | LeaveGame
             | SubscribeGame
             | UnsubscribeGame
             | ShowSubscription
             | ShowSubGame
             | SubmitRule
             | SubmitRuleI
             | Constitution
             | ShowAllRules
             | ListPlayers
             | ShowPendingActions
             | ShowMyPendingActions
             | ShowCompletedActions
             | DoMyActions
             | DoAction
             | Amend
             | Help
             | QuitNomic
             deriving (Eq, Show)

-- | Command's Strings
commands = [--("newPlayer",         NewPlayer,    ": Internal command."),
            ("listGame",          ListGame,     ": list the active games"),
--            ("name",              Name,         " <yourname>: change own name"),
            ("newGame",           NewGame,      " <gamename>: start a new game"),
            ("joinGame",          JoinGame,     " <gamename>: play in a game (you must be subscribed)"),
            ("leaveGame",         LeaveGame,     " <gamename>: stop playing in a game (you remain subscribed)"),
            ("subscribeGame",     SubscribeGame, " <gamename>: subscribe to an existing game"),
            ("unsubscribeGame",   UnsubscribeGame," <gamename>: unsubscribe to an existing game"),
            ("showSubscribtion",  ShowSubscription, " <gamename>: show the subscribtions in the current game"),
            ("showSubGame",       ShowSubGame,   " <gamename>: show the subscribtions to that game"),
            ("submitRule",        SubmitRule,   " <name> <text> <code>: submit a rule. your code must be in \"\""),
            ("iSubmitRule",       SubmitRuleI,  " submit a rule in interactive mode"),
            ("showConstitution",  Constitution, ": show the constitution (must be in a game)"),
            ("showAllRules    ",  ShowAllRules, ": show every rules: Active (the current Constitution), Pending (currently proposed rules), Rejected (Proposed and rejected rules) and Suppressed (Once Active but suppressed rules)"),            
            ("listPlayers",       ListPlayers,  ": show the list of the players"),
            ("amendConstitution", Amend,        ": amend the constitution with the currently proposed rules"),
            ("showPendingActions", ShowPendingActions, ": show all actions that has to be completed by players"), --TODO: verify "by players"
            ("showMyPendingActions", ShowMyPendingActions, ": show all actions that has to be completed by you"),
            ("iDoActions",        DoMyActions,  ": realize now all your actions, in interactive mode"),
            ("doAction",          DoAction,     " <num> <result>: give the result of your action n°num (the number is given by showmypendingactions)."),
            ("showCompletedActions", ShowCompletedActions, ": show all already completed actions"),
            ("help",              Help,         ": show an help message"),
            ("quit",              QuitNomic,    ": quit Nomic")]

            


			
-- | parseLine analysis the line and return a command with optionnal arguments 
parseLine :: String -> Either ParseError (String, [String])
parseLine = parse line "parse error"

-- | line is the parsec parser to analyse a command with optionnal arguments
line :: Parser (String, [String])
line = do skipMany space
          command <- simpleArg
          skipMany space
          args <- (quotedArg <|> simpleArg) `sepBy` spaces
          skipMany space  --TODO: trailing spaces
          return (command, args)
          
-- | a simple argument's parser.
simpleArg :: Parser String
simpleArg = many1 $ noneOf " \n"

-- | a rule parser.
quotedArg :: Parser String
quotedArg = between (char '"') (char '"') $ many $ noneOf ('"' : ['\NUL'..'\US'])



-- | runLine takes a String a executes the represented command, if any. 
runLine :: String -> PlayerNumber -> Comm ()
runLine s pn = do 
   let pl = parseLine s
   case pl of
      Left _ -> putCom "Command analysis error"
      Right (comm, args) -> runCommand comm args pn
   putCom "\n"


-- | runCommand takes a String representing a command, optionnal arguments and runs it.
runCommand :: String -> [String] -> PlayerNumber -> Comm ()
runCommand comm args pn = do
   let comm' = toLowerS comm
   let commands' = map (\(a,b,_) -> (toLowerS a, b)) commands
   --lookup the commands...
   case lookup comm' commands' of
      Just c -> runCommand' c args pn
      Nothing -> do
         --if not found, try abbreviations
         let mycomms = filter (\(sc, _) -> comm' `isPrefixOf` sc) commands'
         case mycomms of
            []         -> putCom "No command matchs your input"
            (_ , c):[] -> runCommand' c args pn
            _ -> putCom $ "Several commands match your input:\n\r" ++ concatMap (\(cs, _) -> cs ++ "\n\r") mycomms


-- | commandMulti takes a command and optionnal arguments and runs it.	   
runCommand' :: Command -> [String] -> PlayerNumber -> Comm ()
runCommand' ListGame _                     = listGame
--runCommand' Name (a:[])                    = newName a
runCommand' NewGame (g:[])                 = newGame g
runCommand' JoinGame (g:[])                = joinGame g
runCommand' LeaveGame _                    = leaveGame
runCommand' SubscribeGame (g:[])           = subscribeGame g
runCommand' UnsubscribeGame (g:[])         = unsubscribeGame g
runCommand' ShowSubscription _             = showSubscribtion
runCommand' ShowSubGame (g:[])             = showSubGame g
runCommand' SubmitRule (name:text:rule:[]) = submitRule name text rule
runCommand' SubmitRuleI _                  = myCatch submitRuleI
runCommand' Constitution _                 = showConstitution
runCommand' ShowAllRules _                 = showAllRules
runCommand' ListPlayers _                  = listPlayers
runCommand' Amend _                        = amendConstitution
runCommand' ShowPendingActions _           = showPendingActions
runCommand' ShowMyPendingActions _         = showMyPendingActions
runCommand' DoMyActions _                  = doActionsI
runCommand' DoAction (num:result:[])       = doAction num result
runCommand' ShowCompletedActions _         = showCompletedActions
runCommand' QuitNomic _                    = quit
runCommand' Help _                         = const help
runCommand' c _                            = const $ putCom $ "the number of arguments doesn't match. \nUsage: \n" ++ getCommandUsage c				   

getCommandUsage :: Command -> String
getCommandUsage c = fromMaybe (error "getCommandUsage: Usage not found") $ lookup c $ map (\(a,b,c) -> (b,a++c)) commands

-- | issue an help message
help :: Comm ()
help = putCom $ "Nomic commands:\n" ++ concatMap (\(c, _, h) -> c ++"\t" ++ h ++ "\n\r") (tail commands)
-}
