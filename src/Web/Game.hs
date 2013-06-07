
{-# LANGUAGE GADTs, OverloadedStrings, ExtendedDefaultRules, DoAndIfThenElse#-}

module Web.Game where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes.RouteT
import Text.Blaze.Internal
import Control.Monad
import Control.Monad.State
import Data.Monoid
import Control.Concurrent.STM
import Language.Nomyx
import Language.Nomyx.Game
import Data.Maybe
import Text.Reform.Happstack
import Text.Reform
import Happstack.Server
import qualified Web.Help as Help
import Web.Common
import Types as T
import Web.Routes.Happstack()
import qualified Text.Reform.Blaze.String as RB hiding (form)
import Control.Applicative
import Utils
import Mail
import Text.Printf
import Data.String
import Data.Text(Text)
import qualified Text.Reform.Blaze.Common as RBC
import qualified Language.Haskell.HsColour.HTML as HSC
import Language.Haskell.HsColour.Colourise hiding (string)
import Multi as M
default (Integer, Double, Data.Text.Text)


viewGame :: Game -> PlayerNumber -> (Maybe SubmitRule) -> RoutedNomyxServer Html
viewGame g pn sr = do
   let inGame = isJust $ Utils.getPlayers g pn
   rf <- viewRuleForm sr inGame
   vi <- viewInputs pn $ _events g
   ok $ table $ do
      td ! A.id "gameCol" $ do
         table $ do
            tr $ td $ h3 $ string $ "Viewing game: " ++ _gameName g
            tr $ td $ "Description:" >> br >> string (_desc $ _gameDesc g) >> br
            tr $ td $ (a "Agora" ! (A.href $ toValue (_agora $ _gameDesc g))) >> br >> br
            tr $ td $ viewPlayers $ _players g
            tr $ td $ viewVictory g
      td ! A.id "gameElem" $ do
         table $ do
         tr $ td $ div ! A.id "rules" $ viewAllRules g
         tr $ td $ div ! A.id "inputs"    ! A.title (toValue Help.inputs)    $ vi
         tr $ td $ div ! A.id "events"    ! A.title (toValue Help.events)    $ viewEvents $ _events g
         tr $ td $ div ! A.id "variables" ! A.title (toValue Help.variables) $ viewVars   $ _variables g
         tr $ td $ div ! A.id "newRule" $ rf
         tr $ td $ div ! A.id "outputs"   ! A.title (toValue Help.outputs)   $ viewOutput (_outputs g) pn

viewPlayers :: [PlayerInfo] -> Html
viewPlayers pis = do
   h5 "Players in game:"
   table $ mapM_ viewPlayer (sort pis)


viewPlayer :: PlayerInfo -> Html
viewPlayer pi = tr $ do
    td $ string $ show $ _playerNumber pi
    td $ string $ _playerName pi

viewVictory :: Game -> Html
viewVictory g = do
    let vs = _playerName <$> mapMaybe (Utils.getPlayers g) (_victory g)
    case vs of
        []   -> br
        a:[] -> h3 $ string $ "Player " ++ (show a) ++ " won the game!"
        a:bs -> h3 $ string $ "Players " ++ (concat $ intersperse ", " $ bs) ++ " and " ++ a ++ " won the game!"

viewAllRules :: Game -> Html
viewAllRules g = do
   h3 "Rules"
   viewRules "Active rules"     (activeRules g) True ! (A.title $ toValue Help.actives) >> br
   viewRules "Pending rules"    (pendingRules g) True ! (A.title $ toValue Help.pendings) >> br
   viewRules "Suppressed rules" (rejectedRules g) False >> br

viewRules :: String -> [Rule] -> Bool -> Html
viewRules title nrs visible = do
   showHideTitle title visible (length nrs == 0) (h4 ! A.style "text-align:center;" $ toHtml title ) $ table ! A.class_ "table" $ do
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
   td ! A.class_ "td" $ string . show $ _rNumber nr
   td ! A.class_ "td" $ string $ _rName nr
   td ! A.class_ "td" $ string $ _rDescription nr
   td ! A.class_ "td" $ string $ if _rProposedBy nr == 0 then "System" else "Player " ++ (show $ _rProposedBy nr)
   td ! A.class_ "td" $ viewRuleFunc $ nr
   td ! A.class_ "td" $ string $ case _rAssessedBy nr of
      Nothing -> "Not assessed"
      Just 0  -> "System"
      Just a  -> "Rule " ++ (show $ a)

viewRuleFunc :: Rule -> Html
viewRuleFunc nr = do
      let code = preEscapedString $ HSC.hscolour defaultColourPrefs False $ _rRuleCode nr
      let ref = "openModalCode" ++ (show $ _rNumber nr)
      div ! A.id "showCodeLink" $ a ! (A.href $ toValue $ "#" ++ ref)  $ "show code" >> br
      code
      div ! A.id (toValue ref) ! A.class_ "modalDialog" $ do
         div $ do
            p $ "Code of the rule:"
            a ! A.href "#close" ! A.title "Close" ! A.class_ "close" $ "X"
            code

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

viewEvents :: [EventHandler] -> Html
viewEvents ehs = do
   showHideTitle "Events" False (length ehs == 0) (h3 "Events") $ table ! A.class_ "table" $ do
         thead $ do
            td ! A.class_ "td" $ text "Event Number"
            td ! A.class_ "td" $ text "By Rule"
            td ! A.class_ "td" $ text "Event"
            --td ! A.class_ "td" $ text "Status"
         mapM_ viewEvent $ sort ehs


viewEvent :: EventHandler -> Html
viewEvent (EH eventNumber ruleNumber event _ _) = tr $ do
   td ! A.class_ "td" $ string . show $ eventNumber
   td ! A.class_ "td" $ string . show $ ruleNumber
   td ! A.class_ "td" $ string . show $ event
   --td ! A.class_ "td" $ string . show $ status

viewInputs :: PlayerNumber -> [EventHandler] -> RoutedNomyxServer Html
viewInputs pn ehs = do
   mis <- mapM (viewInput pn) $ sort ehs
   let is = catMaybes mis
   ok $ showHideTitle "Inputs" True (length is == 0) (h3 "Inputs") $ table $ mconcat is

viewInput :: PlayerNumber -> EventHandler -> RoutedNomyxServer (Maybe Html)
viewInput me (EH eventNumber _ (InputChoice pn title choices def) _ EvActive) | me == pn = do
    link <- showURL (DoInputChoice eventNumber)
    lf  <- lift $ viewForm "user" $ inputChoiceForm title (map show choices) (show def)
    return $ Just $ tr $ td $ blazeForm lf (link)
viewInput me (EH _ _ (InputString pn title) _ EvActive) | me == pn = do
    link <- showURL (DoInputString title)
    lf  <- lift $ viewForm "user" $ inputStringForm title
    return $ Just $ tr $ td $ blazeForm lf (link)
viewInput _ _ = return Nothing

viewVars :: [Var] -> Html
viewVars vs = do
   showHideTitle "Variables" False (length vs == 0) (h3 "Variables") $ table ! A.class_ "table" $ do
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


newRuleForm :: (Maybe SubmitRule) -> NomyxForm SubmitRule
newRuleForm (Just sr) = newRuleForm' sr
newRuleForm Nothing = newRuleForm' (SubmitRule "" "" "")

newRuleForm' :: SubmitRule -> NomyxForm SubmitRule
newRuleForm' (SubmitRule name desc code) = pure SubmitRule  <*> RB.label "Name: " ++> (RB.inputText name)
                               <*> RB.label "      Short description: " ++> RB.inputText desc
                               <*> RB.label "      Code: " ++> RB.textarea 80 15 code
                                   `RBC.setAttr` A.class_ "code" `RBC.setAttr` A.placeholder "Enter here your rule" `RBC.setAttr` (A.title (toValue Help.code))


viewRuleForm :: (Maybe SubmitRule) -> Bool -> RoutedNomyxServer Html
viewRuleForm sr inGame = do
   link <- showURL NewRule
   lf  <- lift $ viewForm "user" $ newRuleForm sr
   ok $ do
      h3 "Propose a new rule:"
      if inGame then blazeForm lf (link) ! A.disabled ""
      else lf ! A.disabled ""

newRule :: (TVar Session) -> RoutedNomyxServer Html
newRule ts = do
   methodM POST
   s@(T.Session sh _ _) <- liftIO $ readTVarIO ts
   r <- liftRouteT $ eitherForm environment "user" (newRuleForm Nothing)
   link <- showURL MainPage
   pn <- getPlayerNumber ts
   case r of
       Right sr -> do
          webCommand ts $ submitRule sr pn sh
          liftIO $ do
             s' <- readTVarIO ts  --TODO clean this
             gn <- getPlayersGame pn s
             gn' <- getPlayersGame pn s'
             let rs = _rules $ _game $ fromJust gn
             let rs' = _rules $ _game $ fromJust gn'
             when (length rs' > length rs) $ sendMailsNewRule s' sr pn
       (Left _) -> liftIO $ putStrLn $ "cannot retrieve form data"
   seeOther link $ string "Redirecting..."


viewOutput :: [Output] -> PlayerNumber -> Html
viewOutput os pn = do
   let myos = map snd $ filter (\o -> fst o == pn) os
   showHideTitle "Output" True (length myos == 0) (h3 "Output") $ mapM_ viewMessages [myos]

viewMessages :: [String] -> Html
viewMessages = mapM_ (\s -> string s >> br)


newInputChoice :: EventNumber -> (TVar Session) -> RoutedNomyxServer Html
newInputChoice en ts = do
    pn <- getPlayerNumber ts
    s <- liftIO $ atomically $ readTVar ts
    mgn <- liftIO $ getPlayersGame pn s
    let eventHandler = fromJust $ findEvent en (_events $ _game $ fromJust mgn)
    methodM POST
    let (title, choices, def) = getChoices eventHandler
    r <- liftRouteT $ eitherForm environment "user" (inputChoiceForm title choices def)
    link <- showURL MainPage
    case r of
       (Right c) -> do
          webCommand ts $ M.inputChoiceResult en c pn
          seeOther link $ string "Redirecting..."
       (Left _) -> do
          liftIO $ putStrLn $ "cannot retrieve form data"
          seeOther link $ string "Redirecting..."

getChoices :: EventHandler -> (String, [String], String)
getChoices (EH _ _ (InputChoice _ title choices def) _ _) = (title, map show choices, show def)
getChoices _ = error "InputChoice event expected"

newInputString :: String -> (TVar Session) -> RoutedNomyxServer Html
newInputString title ts = do
    methodM POST
    pn <- getPlayerNumber ts
    r <- liftRouteT $ eitherForm environment "user" (inputStringForm title)
    link <- showURL MainPage
    case r of
       (Right c) -> do
          webCommand ts $ M.inputStringResult (InputString pn title) c pn
          seeOther link $ string "Redirecting..."
       (Left _) -> do
          liftIO $ putStrLn $ "cannot retrieve form data"
          seeOther link $ string "Redirecting..."


inputChoiceForm :: String -> [String] -> String -> NomyxForm Int
inputChoiceForm title choices def = RB.label (title ++ " ") ++> inputRadio' (zip [0..] choices) ((==) $ fromJust $ elemIndex def choices) <++ RB.label " "

inputStringForm :: String -> NomyxForm String
inputStringForm title = RB.label (title ++ " ") ++> RB.inputText ""

showHideTitle :: String -> Bool -> Bool -> Html -> Html -> Html
showHideTitle id visible empty title rest = do
   div ! A.onclick (fromString $ printf "toggle_visibility('%sBody', '%sShow')" id id) $ table ! A.width "100%" $ tr $ do
      td $ title ! A.width "80%"
      td ! A.style "text-align:right;" $ h5 (if visible then "[Hide]" else "[Show]") ! A.id (fromString $ printf "%sShow" id) ! A.width "20%"
   div ! A.id (fromString $ printf "%sBody" id) ! A.style (fromString $ "display:" ++ (if visible then "block;" else "none;")) $
      if (empty) then (toHtml $ "No " ++ id) else rest

joinGame :: (TVar Session) -> GameName -> RoutedNomyxServer Response
joinGame ts gn = do
   pn <- getPlayerNumber ts
   webCommand ts (M.joinGame gn pn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

leaveGame :: (TVar Session) -> GameName -> RoutedNomyxServer Response
leaveGame ts gn = do
   pn <- getPlayerNumber ts
   webCommand ts (M.leaveGame gn pn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

viewGamePlayer :: (TVar Session) -> GameName -> RoutedNomyxServer Response
viewGamePlayer ts gn = do
   pn <- getPlayerNumber ts
   webCommand ts (M.viewGamePlayer gn pn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."
