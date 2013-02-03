
{-# LANGUAGE GADTs, OverloadedStrings, ExtendedDefaultRules#-}

module Web.Game where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (dir)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes.RouteT
import Text.Blaze.Internal
import Game
import Control.Monad
import Control.Monad.State
import Data.Monoid
import Data.String
import Control.Concurrent.STM
import Language.Nomyx.Expression
import Language.Nomyx.Evaluation
import Data.Maybe
import Text.Reform.Happstack
import Text.Reform
import Happstack.Server
import Data.List
import qualified Web.Help as Help
import Serialize
import Multi --TODO to remove
import Web.Common
import Types
import Web.Routes.Happstack()
import qualified Text.Reform.Blaze.String as RB hiding (form)
import Control.Applicative
import Data.Text(Text)
import qualified Text.Reform.Blaze.Common as RBC
default (Integer, Double, Data.Text.Text)

data NewRuleForm = NewRuleForm { ruleName :: String,
                         ruleText :: String,
                         ruleCode :: String}

viewGame :: Game -> PlayerNumber -> RoutedNomyxServer Html
viewGame g pn = do
   rf <- viewRuleForm pn
   vi <- viewInputs pn $ events g
   ok $ table $ do
      td ! A.id "gameCol" $ do
         table $ do
            tr $ td $ h3 $ string $ "Viewing game: " ++ gameName g  --div ! A.id "gameName"
            tr $ td $ viewPlayers $ players g -- div ! A.id "citizens"
            tr $ td $ viewVictory g --  div ! A.id "victory"
      td ! A.id "gameElem" $ do
         table $ do
         tr $ td $ div ! A.id "rules" $ viewAllRules g
         tr $ td $ div ! A.id "inputs" ! A.title (toValue Help.inputs) $ vi
         tr $ td $ div ! A.id "events" ! A.title (toValue Help.events) $ viewEvents $ events g
         tr $ td $ div ! A.id "variables" ! A.title (toValue Help.variables)$ viewVars $ variables g
         tr $ td $ div ! A.id "newRule" $ rf
         tr $ td $ div ! A.id "outputs" ! A.title (toValue Help.outputs)$ viewOutput (outputs g) pn

viewPlayers :: [PlayerInfo] -> Html
viewPlayers pis = do
   h5 "Players in game:"
   table $ mapM_ viewPlayer (sort pis)


viewPlayer :: PlayerInfo -> Html
viewPlayer pi = tr $ do
    td $ string $ show $ playerNumber pi
    td $ string $ playerName pi

viewVictory :: Game -> Html
viewVictory g = do
    let vs = mapMaybe (getPlayersNameMay g) (victory g)
    case vs of
        []   -> br
        a:[] -> h3 $ string $ "Player " ++ (show a) ++ " won the game!"
        a:bs -> h3 $ string $ "Players " ++ (concat $ intersperse ", " $ bs) ++ " and " ++ a ++ " won the game!"

viewAllRules :: Game -> Html
viewAllRules g = do
   h3 "Rules"
   viewRules "Active rules:" (activeRules g) ! (A.title $ toValue Help.actives)
   viewRules "Pending rules:" (pendingRules g) ! (A.title $ toValue Help.pendings)
   viewRules "Suppressed rules:" $ rejectedRules g

viewRules :: Html -> [Rule] -> Html
viewRules _ [] = return ()
viewRules title nrs = do
   table ! A.class_ "table" $ do
      caption $ h4 title
      thead $ do
         td ! A.class_ "td" $ text "Number"
         td ! A.class_ "td" $ text "Name"
         td ! A.class_ "td" $ text "Description"
         td ! A.class_ "td" $ text "Proposed by"
         td ! A.class_ "td" $ text "Code of the rule"
         td ! A.class_ "td" $ text "Assessed by"
      forM_ nrs viewRule

viewRule :: Rule -> Html
viewRule nr = tr $ do
   td ! A.class_ "td" $ string . show $ rNumber nr
   td ! A.class_ "td" $ string $ rName nr
   td ! A.class_ "td" $ string $ rDescription nr
   td ! A.class_ "td" $ string $ if rProposedBy nr == 0 then "System" else "Player " ++ (show $ rProposedBy nr)
   td ! A.class_ "td" $ string $ rRuleCode nr
   td ! A.class_ "td" $ string $ case rAssessedBy nr of
      Nothing -> "Not assessed"
      Just 0  -> "System"
      Just a  -> "Rule " ++ (show $ a)

viewEvents :: [EventHandler] -> Html
viewEvents [] = h3 "Events" >> h5 "No Events"
viewEvents ehs = do
   h3 "Events"
   table ! A.class_ "table" $ do
      thead $ do
         td ! A.class_ "td" $ text "Event Number"
         td ! A.class_ "td" $ text "By Rule"
         td ! A.class_ "td" $ text "Event"
      mapM_ viewEvent $ sort ehs

viewEvent :: EventHandler -> Html
viewEvent (EH eventNumber ruleNumber event _) = tr $ do
   td ! A.class_ "td" $ string . show $ eventNumber
   td ! A.class_ "td" $ string . show $ ruleNumber
   td ! A.class_ "td" $ string . show $ event

viewInputs :: PlayerNumber -> [EventHandler] -> RoutedNomyxServer Html
viewInputs pn ehs = do
   mis <- mapM (viewInput pn) $ sort ehs
   let is = catMaybes mis
   case length is of
      0 -> ok $ h3 "Inputs" >> h5 "No Inputs"
      _ -> ok $ do
         h3 "Inputs"
         table $ do
            mconcat is

viewInput :: PlayerNumber -> EventHandler -> RoutedNomyxServer (Maybe Html)
viewInput me (EH eventNumber _ (InputChoice pn title choices def) _) | me == pn = do
    link <- showURL (DoInputChoice pn eventNumber)
    lf  <- lift $ viewForm "user" $ inputChoiceForm title (map show choices) (show def)
    return $ Just $ tr $ td $ blazeForm lf (link)
viewInput me (EH _ _ (InputString pn title) _) | me == pn = do
    link <- showURL (DoInputString pn title)
    lf  <- lift $ viewForm "user" $ inputStringForm title
    return $ Just $ tr $ td $ blazeForm lf (link)
viewInput _ _ = return Nothing

viewVars :: [Var] -> Html
viewVars [] = h3 "Variables" >> h5 "No Variables"
viewVars vs = do
   h3 "Variables"
   table ! A.class_ "table" $ do
      thead $ do
         td ! A.class_ "td" $ text "Rule number"
         td ! A.class_ "td" $ text "Name"
         td ! A.class_ "td" $ text "Value"
      mapM_ viewVar vs

viewVar :: Var -> Html
viewVar (Var vRuleNumber vName vData) = tr $ do
   td ! A.class_ "td" $ string . show $ vRuleNumber
   td ! A.class_ "td" $ string . show $ vName
   td ! A.class_ "td" $ string . show $ vData


newRuleForm :: NomyxForm NewRuleForm
newRuleForm = pure NewRuleForm <*> RB.label "Name: " ++> (RB.inputText "")
                               <*> RB.label "      Short description: " ++> RB.inputText ""
                               <*> RB.label "Code: " ++> RB.inputText ""
                                   `RBC.setAttr` A.name "code" `RBC.setAttr` A.id "code" `RBC.setAttr` A.placeholder "Enter here your rule" `RBC.setAttr` (A.title (toValue Help.code))

viewRuleForm :: PlayerNumber -> RoutedNomyxServer Html
viewRuleForm pn = do
   link <- showURL (NewRule pn)
   lf  <- lift $ viewForm "user" $ newRuleForm
   ok $ do
      h3 "Propose a new rule:"
      blazeForm lf (link)


viewOutput :: [Output] -> PlayerNumber -> Html
viewOutput [] _ = h3 "Output" >> h5 "No Output"
viewOutput os pn = do
   h3 "Output"
   let myos = map snd $ filter (\o -> fst o == pn) os
   mapM_ viewMessages [myos]

viewMessages :: [String] -> Html
viewMessages = mapM_ (\s -> string s >> br)


newInputChoice :: PlayerNumber -> EventNumber -> (TVar Multi) -> RoutedNomyxServer Html
newInputChoice pn en tm = do
    multi <- liftRouteT $ lift $ atomically $ readTVar tm
    let mg = fromJust $ getPlayersGame pn multi
    let eventHandler = fromJust $ findEvent en (events mg)
    methodM POST
    let (title, choices, def) = getChoices eventHandler
    r <- liftRouteT $ eitherForm environment "user" (inputChoiceForm title choices def)
    link <- showURL $ Noop pn
    case r of
       (Right c) -> do
          liftRouteT $ lift $ putStrLn $ "choice:" ++ (show c)
          execCommand tm $ update $ MultiInputChoiceResult en c pn
          seeOther link $ string "Redirecting..."
       (Left _) -> do
          liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
          seeOther link $ string "Redirecting..."

getChoices :: EventHandler -> (String, [String], String)
getChoices (EH _ _ (InputChoice _ title choices def) _) = (title, map show choices, show def)
getChoices _ = error "InputChoice event expected"

newInputString :: PlayerNumber -> String -> (TVar Multi) -> RoutedNomyxServer Html
newInputString pn title tm = do
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" (inputStringForm title)
    link <- showURL $ Noop pn
    case r of
       (Right c) -> do
          liftRouteT $ lift $ putStrLn $ "entered:" ++ (show c)
          execCommand tm $ update $ MultiInputStringResult title c pn
          seeOther link $ string "Redirecting..."
       (Left _) -> do
          liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
          seeOther link $ string "Redirecting..."


inputChoiceForm :: String -> [String] -> String -> NomyxForm Int
inputChoiceForm title choices def = RB.label (title ++ " ") ++> inputRadio' (zip [0..] choices) ((==) $ fromJust $ elemIndex def choices)

inputStringForm :: String -> NomyxForm String
inputStringForm title = RB.label (title ++ " ") ++> RB.inputText ""
