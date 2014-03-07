{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Web.Game where

import Prelude hiding (div)
import qualified Prelude
import Control.Monad
import Control.Monad.State
import Control.Concurrent.STM
import Control.Applicative
import Control.Category hiding ((.))
import Data.Monoid
import Data.Maybe
import Data.String
import Data.List
import Data.Text (Text)
import Data.Typeable
import Data.Time
import Data.Lens
import Text.Printf
import System.Locale
import Language.Nomyx
import Language.Nomyx.Engine
import Text.Blaze.Html5                    (Html, div, (!), p, table, thead, td, tr, h2, h3, h4, h5, pre, toValue, br, toHtml, a, img)
import Text.Blaze.Html5.Attributes as A    (src, title, width, style, id, onclick, disabled, placeholder, class_, href)
import Text.Blaze.Internal                 (string, text)
import Text.Reform.Blaze.String as RB      (label, inputText, textarea, inputSubmit, inputCheckboxes, inputHidden)
import Text.Reform.Happstack               (environment)
import Text.Reform                         ((<++), (++>), viewForm, eitherForm)
import Text.Reform.Blaze.Common            (setAttr)
import Happstack.Server                    (Response, Method(..), seeOther, toResponse, methodM, ok)
import Web.Routes.RouteT                   (showURL, liftRouteT)
import qualified Web.Help as Help
import Types as T
import Mail
import Session as S
import Web.Common
import Safe
import Profile
default (Integer, Double, Data.Text.Text)

viewGame :: Game -> PlayerNumber -> (Maybe LastRule) -> Bool -> RoutedNomyxServer Html
viewGame g pn mlr isAdmin = do
   let pi = Profile.getPlayerInfo g pn
   let isGameAdmin = isAdmin || (if (isJust $ _simu g) then ((_ownedBy $ fromJust $ _simu g) == pn) else False)
   let playAs = if (isJust pi) then (_playAs $ fromJust pi) else Nothing
   rf <- viewRuleForm mlr (isJust pi) isAdmin (_gameName g)
   vios <- viewIOs (fromMaybe pn playAs) g
   vgd <- viewGameDesc g playAs isGameAdmin
   ok $ table $ do
      tr $ td $ div ! A.id "gameDesc" $ vgd
      tr $ td $ div ! A.id "rules"    $ viewAllRules g
      tr $ td $ div ! A.id "ios"      $ vios
      tr $ td $ div ! A.id "newRule"  $ rf
      tr $ td $ div ! A.id "details"  $ viewDetails pn g

viewGameDesc :: Game -> Maybe PlayerNumber -> Bool -> RoutedNomyxServer Html
viewGameDesc g playAs gameAdmin = do
   vp <- viewPlayers (_players g) (_gameName g) gameAdmin
   paf <- lift $ viewForm "user" $ playAsForm $ Nothing
   ok $ do
      p $ do
        h3 $ string $ "Viewing game: " ++ _gameName g
        when (isJust playAs) $ do
           h4 $ string $ "You are playing as player " ++ (show $ fromJust playAs) ++ ". Cancel:"
           paf
      p $ do
         h4 $ "Description:"
         string (_desc $ _gameDesc g)
      p $ h4 $ "This game is discussed in the " >> a "Agora" ! (A.href $ toValue (_agora $ _gameDesc g)) >> "."
      p $ h4 $ "Players in game:"
      when gameAdmin $ "(click on the player's name to \"play as\" this player)"
      vp
      p $ viewVictory g


viewPlayers :: [PlayerInfo] -> GameName -> Bool -> RoutedNomyxServer Html
viewPlayers pis gn gameAdmin = do
   vp <- mapM (viewPlayer gn gameAdmin) (sort pis)
   ok $ table $ mconcat vp
      --let plChunks = transpose $ chunksOf (1 + (length pis) `Prelude.div` 3) (sort pis)
      --table $ mapM_ (\row -> tr $ mapM_ (viewPlayer pn) row) plChunks


viewPlayer :: GameName -> Bool -> PlayerInfo -> RoutedNomyxServer Html
viewPlayer gn gameAdmin (PlayerInfo pn name _) = do
   pad <- playAsDiv pn gn
   ok $ tr $ do
    let inf = string ((show pn) ++ "\t" ++ name)
    pad
    if gameAdmin
       then td $ a inf ! (href $ toValue $ "#openModalPlayAs" ++ (show pn))
       else td inf

playAsDiv :: PlayerNumber -> GameName -> RoutedNomyxServer Html
playAsDiv pn gn = do
   submitPlayAs <- showURL $ SubmitPlayAs gn
   main  <- showURL MainPage
   paf <- lift $ viewForm "user" $ playAsForm $ Just pn
   ok $ do
      let cancel = a "Cancel" ! (href $ toValue main) ! A.class_ "modalButton"
      div ! A.id (toValue $ "openModalPlayAs" ++ (show pn)) ! A.class_ "modalWindow" $ do
         div $ do
            h2 "When you own a game, you can play instead of any players. This allows you to test \
                the result of the corresponding actions."
            blazeForm (h2 (string $ "Play as player " ++ (show pn) ++ "?  ") >> paf) submitPlayAs
            br
            cancel

playAsForm :: Maybe PlayerNumber -> NomyxForm String
playAsForm pn = inputHidden (show pn)


viewVictory :: Game -> Html
viewVictory g = do
    let vs = _playerName <$> mapMaybe (Profile.getPlayerInfo g) (getVictorious g)
    case vs of
        []   -> br
        a:[] -> h3 $ string $ "Player " ++ (show a) ++ " won the game!"
        a:bs -> h3 $ string $ "Players " ++ (concat $ intersperse ", " $ bs) ++ " and " ++ a ++ " won the game!"

viewAllRules :: Game -> Html
viewAllRules g = do
   titleWithHelpIcon (h3 "Rules") Help.rules
   viewRules (activeRules g)   "Active rules"     True g >> br
   viewRules (pendingRules g)  "Pending rules"    True g >> br
   viewRules (rejectedRules g) "Suppressed rules" False g >> br

viewRules :: [Rule] -> String -> Bool -> Game -> Html
viewRules nrs title visible g = do
   showHideTitle title visible (length nrs == 0) (h4 $ toHtml (title ++ ":") ) $ table ! class_ "table" $ do
      thead $ do
         td ! class_ "td" $ text "#"
         td ! class_ "td" $ text "Name"
         td ! class_ "td" $ text "Description"
         td ! class_ "td" $ text "Proposed by"
         td ! class_ "td" $ text "Code of the rule"
         td ! class_ "td" $ text "Assessed by"
      forM_ nrs (viewRule g)

viewRule :: Game -> Rule -> Html
viewRule g nr = tr $ do
   let pl = fromMaybe ("Player " ++ (show $ _rProposedBy nr)) (_playerName <$> (Profile.getPlayerInfo g $ _rProposedBy nr))
   td ! class_ "td" $ string . show $ _rNumber nr
   td ! class_ "td" $ string $ _rName nr
   td ! class_ "td" $ string $ _rDescription nr
   td ! class_ "td" $ string $ if _rProposedBy nr == 0 then "System" else pl
   td ! class_ "codetd" $ viewRuleFunc $ nr
   td ! class_ "td" $ string $ case _rAssessedBy nr of
      Nothing -> "Not assessed"
      Just 0  -> "System"
      Just a  -> "Rule " ++ (show $ a)

viewRuleFunc :: Rule -> Html
viewRuleFunc nr = do
   let code = displayCode $ _rRuleCode nr
   let ref = "openModalCode" ++ (show $ _rNumber nr)
   div ! A.id "showCodeLink" $ a ! (href $ toValue $ "#" ++ ref)  $ "show code" >> br
   code
   div ! A.id (toValue ref) ! class_ "modalDialog" $ do
      div $ do
         p $ "Code of the rule:"
         a ! href "#close" ! title "Close" ! class_ "close" $ "X"
         div ! A.id "modalCode" $ code

viewDetails :: PlayerNumber -> Game -> Html
viewDetails pn g = showHideTitle "Details" False False (h3 "Details") $ do
   p $ titleWithHelpIcon (h4 "Variables:") Help.variables
   viewVars   (_variables g)
   p $ titleWithHelpIcon (h4 "Events:") Help.events
   viewEvents (_events g)
   p $ h4 "Log:"
   viewLogs    (_logs g) pn


viewEvents :: [EventHandler] -> Html
viewEvents ehs = table ! class_ "table" $ do
         thead $ do
            td ! class_ "td" $ text "Event Number"
            td ! class_ "td" $ text "By Rule"
            td ! class_ "td" $ text "Event"
         mapM_ viewEvent $ sort ehs


viewEvent :: EventHandler -> Html
viewEvent (EH eventNumber ruleNumber event _ status) = if status == SActive then disp else disp ! style "background:gray;" where
   disp = tr $ do
      td ! class_ "td" $ string . show $ eventNumber
      td ! class_ "td" $ string . show $ ruleNumber
      td ! class_ "td" $ string . show $ event

viewIOs :: PlayerNumber -> Game -> RoutedNomyxServer Html
viewIOs pn g = do
   vios <- mapM (viewIORule pn g) (sort $ _rules g)
   ok $ do
      titleWithHelpIcon (h3 "Inputs/Ouputs") Help.inputsOutputs
      a "" ! A.id (toValue inputAnchor)
      mconcat vios

viewIORule :: PlayerNumber -> Game -> Rule -> RoutedNomyxServer Html
viewIORule pn g r = do
   vior <- viewIORuleM pn (_rNumber r) g
   ok $ when (isJust vior) $ div ! A.id "IORule" $ do
      div ! A.id "IORuleTitle" $ h4 $ string $ "IO for Rule \"" ++ (_rName r) ++ "\" (#" ++ (show $ _rNumber r) ++ "):"
      fromJust vior


viewIORuleM :: PlayerNumber -> RuleNumber -> Game -> RoutedNomyxServer (Maybe Html)
viewIORuleM pn rn g = do
   vir <- viewInputsRule pn rn (_events g) (_gameName g)
   let vor = viewOutputsRule pn rn g
   if (isJust vir || isJust vor) then return $ Just $ do
      when (isJust vir) $ fromJust vir
      when (isJust vor) $ fromJust vor
   else return Nothing

viewInputsRule :: PlayerNumber -> RuleNumber -> [EventHandler] -> GameName -> RoutedNomyxServer (Maybe Html)
viewInputsRule pn rn ehs gn = do
   let filtered = filter (\e -> _ruleNumber e == rn) ehs
   mis <- mapM (viewInput pn gn) $ sort filtered
   let is = catMaybes mis
   case is of
      [] -> return Nothing
      i -> return $ Just $ table $ mconcat i

viewOutputsRule :: PlayerNumber -> RuleNumber -> Game -> (Maybe Html)
viewOutputsRule pn rn g = do
   tracePN 0 "in viewOutputsRule"
   let filtered = filter (\o -> _oRuleNumber o == rn) (_outputs g)
   let myos = filter (isPn pn) (reverse filtered)
   case myos of
      [] -> Nothing
      os -> Just $ mapM_ (viewOutput g) os

isPn pn (Output _ _ (Just mypn) _ SActive) = mypn == pn
isPn _ (Output _ _ Nothing _ SActive) = True
isPn _ _ = False

viewInput :: PlayerNumber -> GameName -> EventHandler -> RoutedNomyxServer (Maybe Html)
viewInput me gn (EH eventNumber _ (InputEv (Input pn title iForm)) _ SActive) | me == pn = do
    link <- showURL (DoInput eventNumber gn)
    lf  <- lift $ viewForm "user" $ inputForm iForm
    return $ Just $ tr $ td $ do
       string $ title
       string " "
       blazeForm lf (link) ! A.id "InputForm"
viewInput _ _ _ = return Nothing

viewOutput :: Game -> Output -> Html
viewOutput g o = pre $ string (evalOutput g o) >> br

viewVars :: [Var] -> Html
viewVars vs = table ! class_ "table" $ do
      thead $ do
         td ! class_ "td" $ text "Rule number"
         td ! class_ "td" $ text "Name"
         td ! class_ "td" $ text "Value"
      mapM_ viewVar vs

viewVar :: Var -> Html
viewVar (Var vRuleNumber vName vData) = tr $ do
   td ! class_ "td" $ string . show $ vRuleNumber
   td ! class_ "td" $ string . show $ vName
   td ! class_ "td" $ string . show $ vData


newRuleForm :: (Maybe SubmitRule) -> Bool -> NomyxForm (SubmitRule, Maybe String, Maybe String)
newRuleForm (Just sr) isAdmin = newRuleForm' sr isAdmin
newRuleForm Nothing isAdmin = newRuleForm' (SubmitRule "" "" "") isAdmin

newRuleForm' :: SubmitRule -> Bool -> NomyxForm (SubmitRule, Maybe String, Maybe String)
newRuleForm' (SubmitRule name desc code) isAdmin =
   (,,) <$> (SubmitRule <$> label "Name: " ++> (RB.inputText name)
                        <*> label "      Short description: " ++> RB.inputText desc
                        <*> label "      Code: " ++> textarea 80 15 code `setAttr` class_ "code" `setAttr` placeholder "Enter here your rule")
       <*> inputSubmit "Check"
       <*> if isAdmin then inputSubmit "Admin submit" else pure Nothing


viewRuleForm :: Maybe LastRule -> Bool -> Bool -> GameName -> RoutedNomyxServer Html
viewRuleForm mlr inGame isAdmin gn = do
   link <- showURL (NewRule gn)
   lf  <- lift $ viewForm "user" (newRuleForm (fst <$> mlr) isAdmin)
   ok $ do
      a "" ! A.id (toValue ruleFormAnchor)
      titleWithHelpIcon (h3 "Propose a new rule:") Help.code
      if inGame then do
         blazeForm lf (link)
         let msg = snd <$> mlr
         when (isJust msg) $ pre $ string $ fromJust msg
      else lf ! disabled ""

newRule :: (TVar Session) -> GameName -> RoutedNomyxServer Response
newRule ts gn = toResponse <$> do
   methodM POST
   s@(T.Session sh _ _) <- liftIO $ readTVarIO ts
   admin <- getIsAdmin ts
   r <- liftRouteT $ eitherForm environment "user" (newRuleForm Nothing admin)
   link <- showURL MainPage
   pn <- getPlayerNumber ts
   case r of
       Right (sr, Nothing, Nothing) -> do
          webCommand ts $ submitRule sr pn gn sh
          liftIO $ do
             s' <- readTVarIO ts  --TODO clean this
             gn <- getPlayersGame pn s
             gn' <- getPlayersGame pn s'
             let rs = _rules $ _game $ fromJustNote "newRule" gn
             let rs' = _rules $ _game $ fromJustNote "newRule" gn'
             when (length rs' > length rs) $ sendMailsNewRule s' sr pn
       Right (sr, Just _, Nothing) -> webCommand ts $ checkRule sr pn sh
       Right (sr, Nothing, Just _) -> webCommand ts $ adminSubmitRule sr pn gn sh
       Right (_,  Just _, Just _)  -> error "Impossible new rule form result"
       (Left _) -> liftIO $ putStrLn $ "cannot retrieve form data"
   seeOther (link `appendAnchor` ruleFormAnchor) $ string "Redirecting..."

viewLogs :: [Log] -> PlayerNumber -> Html
viewLogs log pn = do
   let ls = filter (\o -> (_lPlayerNumber o == Just pn) || (_lPlayerNumber o == Nothing)) log
   table $ mapM_ viewLog (reverse ls)

viewLog :: Log -> Html
viewLog (Log _ t s) = tr $ do
   td $ string $ formatTime defaultTimeLocale "%Y/%m/%d_%H:%M" t
   td $ p $ string s

newInput :: (TVar Session) -> EventNumber -> GameName -> RoutedNomyxServer Response
newInput ts en gn = toResponse <$> do
    pn <- getPlayerNumber ts
    s <- liftIO $ atomically $ readTVar ts
    let g = find ((== gn) . getL (game >>> gameName)) (_games $ _multi s)
    let eventHandler = getEventHandler en (fromJust g)
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" (getNomyxForm eventHandler)
    link <- showURL MainPage
    case r of
       (Right c) -> do
          webCommand ts $ S.inputResult pn en c gn
       (Left _) -> do
          liftIO $ putStrLn $ "cannot retrieve form data"
    seeOther (link `appendAnchor` inputAnchor) $ string "Redirecting..."


newPlayAs :: (TVar Session) -> GameName -> RoutedNomyxServer Response
newPlayAs ts gn = toResponse <$> do
   methodM POST
   p <- liftRouteT $ eitherForm environment "user" $ playAsForm Nothing
   pn <- getPlayerNumber ts
   case p of
      Right playAs -> do
         webCommand ts $ S.playAs (read playAs) pn gn
         link <- showURL MainPage
         seeOther link $ string "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL $ SubmitPlayAs gn
         mainPage  "Admin settings" "Admin settings" (blazeForm errorForm settingsLink) False True

getNomyxForm :: EventHandler -> NomyxForm UInputData
getNomyxForm (EH _ _ (InputEv (Input _ _ iForm)) _ _) = inputForm iForm
getNomyxForm _ = error "Not an Input Event"

inputForm :: (Typeable a) => InputForm a -> NomyxForm UInputData
inputForm (Radio choices)    = URadioData    <$> inputRadio' (zip [0..] (snd <$> choices)) ((==) 0) <++ label " "
inputForm Text               = UTextData     <$> RB.inputText "" <++ label " "
inputForm TextArea           = UTextAreaData <$> textarea 50 5  "" <++ label " "
inputForm Button             = pure UButtonData
inputForm (Checkbox choices) = UCheckboxData <$> inputCheckboxes (zip [0..] (snd <$> choices)) (const False) <++ label " "

showHideTitle :: String -> Bool -> Bool -> Html -> Html -> Html
showHideTitle id visible empty title rest = do
   div ! onclick (fromString $ printf "toggle_visibility('%sBody', '%sShow')" id id) $ table ! width "100%" $ tr $ do
      td $ title ! width "80%"
      td ! style "text-align:right;" $ h5 (if visible then "[Click to hide]" else "[Click to show]") ! A.id (fromString $ printf "%sShow" id) ! width "20%"
   div ! A.id (fromString $ printf "%sBody" id) ! style (fromString $ "display:" ++ (if visible then "block;" else "none;")) $
      if (empty) then (toHtml $ "No " ++ id) else rest

joinGame :: (TVar Session) -> GameName -> RoutedNomyxServer Response
joinGame ts gn = do
   pn <- getPlayerNumber ts
   webCommand ts (S.joinGame gn pn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

leaveGame :: (TVar Session) -> GameName -> RoutedNomyxServer Response
leaveGame ts gn = do
   pn <- getPlayerNumber ts
   webCommand ts (S.leaveGame gn pn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

delGame :: (TVar Session) -> GameName -> RoutedNomyxServer Response
delGame ts gn = do
   webCommand ts (S.delGame gn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

forkGame :: (TVar Session) -> GameName -> RoutedNomyxServer Response
forkGame ts gn = do
   pn <- getPlayerNumber ts
   webCommand ts $ S.startSimulation gn pn
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

viewGamePlayer :: (TVar Session) -> GameName -> RoutedNomyxServer Response
viewGamePlayer ts gn = do
   pn <- getPlayerNumber ts
   webCommand ts (S.viewGamePlayer gn pn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

titleWithHelpIcon :: Html -> String -> Html
titleWithHelpIcon myTitle help = table ! width "100%" $ tr $ do
   td ! style "text-align:left;" $ myTitle
   td ! style "text-align:right;" $ img ! src "/static/pictures/help.jpg" ! title (toValue help)
