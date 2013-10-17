
{-# LANGUAGE GADTs, OverloadedStrings, ExtendedDefaultRules, DoAndIfThenElse#-}

module Web.Game where

import Prelude hiding (div)
import qualified Prelude as P
import Text.Blaze.Html5 hiding (map, head)
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes.RouteT
import Text.Blaze.Internal hiding (Text)
import Control.Monad
import Control.Monad.State
import Data.Monoid
import Control.Concurrent.STM
import Language.Nomyx
import Language.Nomyx.Game
import Data.Maybe
import Text.Reform.Happstack
import Text.Reform
import Happstack.Server hiding (Input)
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
import Data.List.Split
import Data.Typeable
import Data.Time
import System.Locale
import Data.Lens
import Control.Category hiding ((.))
import Safe
default (Integer, Double, Data.Text.Text)

viewGame :: Game -> PlayerNumber -> (Maybe LastRule) -> Bool -> RoutedNomyxServer Html
viewGame g pn mlr isAdmin = do
   let inGame = isJust $ Utils.getPlayer g pn
   rf <- viewRuleForm mlr inGame isAdmin (_gameName g)
   vios <- viewIOs pn (_rules g) (_events g) (_outputs g) (_gameName g)
   ok $ table $ do
      tr $ td $ div ! A.id "gameDesc" $ viewGameDesc g pn
      tr $ td $ div ! A.id "rules" $ viewAllRules g
      tr $ td $ div ! A.id "ios" $ vios
      tr $ td $ div ! A.id "newRule" $ rf
      tr $ td $ div ! A.id "details" $ viewDetails pn g

viewGameDesc :: Game -> PlayerNumber -> Html
viewGameDesc g pn = do
   p $ h3 $ string $ "Viewing game: " ++ _gameName g
   p $ do
      h4 $ "Description:"
      string (_desc $ _gameDesc g)
   p $ h4 $ "This game is discussed in the " >> a "Agora" ! (A.href $ toValue (_agora $ _gameDesc g)) >> "."
   p $ h4 $ "Players in game:"
   viewPlayers (_players g) pn
   p $ viewVictory g

viewPlayers :: [PlayerInfo] -> PlayerNumber -> Html
viewPlayers pis pn = do
   let plChunks = transpose $ chunksOf (1 + (length pis) `P.div` 3) (sort pis)
   table $ mapM_ (\row -> tr $ mapM_ (viewPlayer pn) row) plChunks


viewPlayer :: PlayerNumber -> PlayerInfo -> Html
viewPlayer mypn (PlayerInfo pn name) = do
    let inf = string ((show pn) ++ "\t" ++ name)
    if mypn == pn
       then td ! A.style "color: red;" $ inf
       else td inf


viewVictory :: Game -> Html
viewVictory g = do
    let vs = _playerName <$> mapMaybe (Utils.getPlayer g) (_victory g)
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
   showHideTitle title visible (length nrs == 0) (h4 $ toHtml (title ++ ":") ) $ table ! A.class_ "table" $ do
      thead $ do
         td ! A.class_ "td" $ text "#"
         td ! A.class_ "td" $ text "Name"
         td ! A.class_ "td" $ text "Description"
         td ! A.class_ "td" $ text "Proposed by"
         td ! A.class_ "td" $ text "Code of the rule"
         td ! A.class_ "td" $ text "Assessed by"
      forM_ nrs (viewRule g)

viewRule :: Game -> Rule -> Html
viewRule g nr = tr $ do
   let pl = fromMaybe ("Player " ++ (show $ _rProposedBy nr)) (_playerName <$> (Utils.getPlayer g $ _rProposedBy nr))
   td ! A.class_ "td" $ string . show $ _rNumber nr
   td ! A.class_ "td" $ string $ _rName nr
   td ! A.class_ "td" $ string $ _rDescription nr
   td ! A.class_ "td" $ string $ if _rProposedBy nr == 0 then "System" else pl
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
            div ! A.id "modalCode"$ code

viewDetails :: PlayerNumber -> Game -> Html
viewDetails pn g = showHideTitle "Details" False False (h3 "Details") $ do
   p $ titleWithHelpIcon (h4 "Variables:") Help.variables
   viewVars   (_variables g)
   p $ titleWithHelpIcon (h4 "Events:") Help.events
   viewEvents (_events g)
   p $ h4 "Log:"
   viewLogs    (_logs g) pn


viewEvents :: [EventHandler] -> Html
viewEvents ehs = table ! A.class_ "table" $ do
         thead $ do
            td ! A.class_ "td" $ text "Event Number"
            td ! A.class_ "td" $ text "By Rule"
            td ! A.class_ "td" $ text "Event"
         mapM_ viewEvent $ sort ehs


viewEvent :: EventHandler -> Html
viewEvent (EH eventNumber ruleNumber event _ status) = if status == SActive then disp else disp ! A.style "background:gray;" where
   disp = tr $ do
      td ! A.class_ "td" $ string . show $ eventNumber
      td ! A.class_ "td" $ string . show $ ruleNumber
      td ! A.class_ "td" $ string . show $ event

viewIOs :: PlayerNumber -> [Rule] -> [EventHandler] -> [Output] -> GameName -> RoutedNomyxServer Html
viewIOs pn rs ehs os gn = do
   vios <- mapM (viewIORule pn ehs os gn) (sort rs)
   ok $ do
      titleWithHelpIcon (h3 "Inputs/Ouputs") Help.inputsOutputs
      mconcat vios

viewIORule :: PlayerNumber -> [EventHandler] -> [Output] -> GameName -> Rule -> RoutedNomyxServer Html
viewIORule pn ehs os gn r = do
   vior <- viewIORuleM pn (_rNumber r) ehs os gn
   ok $ when (isJust vior) $ div ! A.id "IORule" $ do
      div ! A.id "IORuleTitle" $ h4 $ string $ "IO for Rule \"" ++ (_rName r) ++ "\" (#" ++ (show $ _rNumber r) ++ "):"
      fromJust vior


viewIORuleM :: PlayerNumber -> RuleNumber -> [EventHandler] -> [Output] -> GameName -> RoutedNomyxServer (Maybe Html)
viewIORuleM pn rn ehs os gn = do
   vir <- viewInputsRule pn rn ehs gn
   let vor = viewOutputsRule pn rn os
   if (isJust vir || isJust vor) then do
      return $ Just $ do
         when (isJust vir) $ fromJust vir
         when (isJust vor) $ fromJust vor
   else
      return Nothing

viewInputsRule :: PlayerNumber -> RuleNumber -> [EventHandler] -> GameName -> RoutedNomyxServer (Maybe Html)
viewInputsRule pn rn ehs gn = do
   let filtered = filter (\e -> _ruleNumber e == rn) ehs
   mis <- mapM (viewInput pn gn) $ sort filtered
   let is = catMaybes mis
   case is of
      [] -> return Nothing
      i -> return $ Just $ table $ mconcat i

viewOutputsRule :: PlayerNumber -> RuleNumber -> [Output] -> (Maybe Html)
viewOutputsRule pn rn os = do
   let filtered = filter (\o -> _oRuleNumber o == rn) os
   let myos = map _output $ filter (isPn pn) (reverse filtered)
   case myos of
      [] -> Nothing
      os -> Just $ mapM_ viewOutput os

isPn pn (Output _ _ (Just mypn) _ SActive) = mypn == pn
isPn _ (Output _ _ Nothing _ SActive) = True
isPn _ _ = False

viewInput :: PlayerNumber -> GameName -> EventHandler -> RoutedNomyxServer (Maybe Html)
viewInput me gn (EH eventNumber _ (InputEv (Input pn title iForm)) _ SActive) | me == pn = do
    link <- showURL (DoInput eventNumber gn)
    lf  <- lift $ viewForm "user" $ inputForm iForm
    return $ Just $ tr $ td $ do
       string title
       blazeForm lf (link) ! A.id "InputForm"
viewInput _ _ _ = return Nothing

viewOutput :: String -> Html
viewOutput s = pre $ string s >> br

viewVars :: [Var] -> Html
viewVars vs = table ! A.class_ "table" $ do
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


newRuleForm :: (Maybe SubmitRule) -> Bool -> NomyxForm (SubmitRule, Maybe String)
newRuleForm (Just lr) isAdmin = newRuleForm' lr isAdmin
newRuleForm Nothing isAdmin = newRuleForm' (SubmitRule "" "" "") isAdmin

newRuleForm' :: SubmitRule -> Bool -> NomyxForm (SubmitRule, Maybe String)
newRuleForm' (SubmitRule name desc code) isAdmin =
   (,) <$> (SubmitRule <$> RB.label "Name: " ++> (RB.inputText name)
                       <*> RB.label "      Short description: " ++> RB.inputText desc
                       <*> RB.label "      Code: " ++> RB.textarea 80 15 code `RBC.setAttr` A.class_ "code" `RBC.setAttr` A.placeholder "Enter here your rule")
       <*> if isAdmin then RB.inputSubmit "Admin submit" else pure Nothing


viewRuleForm :: Maybe LastRule -> Bool -> Bool -> GameName -> RoutedNomyxServer Html
viewRuleForm msr inGame isAdmin gn = do
   link <- showURL (NewRule gn)
   lf  <- lift $ viewForm "user" (newRuleForm (fst <$> msr) isAdmin)
   ok $ do
      titleWithHelpIcon (h3 "Propose a new rule:") Help.code
      if inGame then do
         blazeForm lf (link)
         let error = snd <$> msr
         when (isJust error) $ do
            h5 $ "Error in submitted rule: "
            pre $ string $ fromJust error
      else lf ! A.disabled ""

newRule :: (TVar Session) -> GameName -> RoutedNomyxServer Response
newRule ts gn = toResponse <$> do
   methodM POST
   s@(T.Session sh _ _) <- liftIO $ readTVarIO ts
   admin <- getIsAdmin ts
   r <- liftRouteT $ eitherForm environment "user" (newRuleForm Nothing admin)
   link <- showURL MainPage
   pn <- getPlayerNumber ts
   case r of
       Right (sr, Nothing) -> do
          webCommand ts $ submitRule sr pn gn sh
          liftIO $ do
             s' <- readTVarIO ts  --TODO clean this
             gn <- getPlayersGame pn s
             gn' <- getPlayersGame pn s'
             let rs = _rules $ _game $ fromJustNote "newRule" gn
             let rs' = _rules $ _game $ fromJustNote "newRule" gn'
             when (length rs' > length rs) $ sendMailsNewRule s' sr pn
       Right (sr, Just _) -> webCommand ts $ adminSubmitRule sr pn gn sh
       (Left _) -> liftIO $ putStrLn $ "cannot retrieve form data"
   seeOther link $ string "Redirecting..."

viewLogs :: [Log] -> PlayerNumber -> Html
viewLogs log pn = do
   let ls = filter (\o -> (_lPlayerNumber o == Just pn) || (_lPlayerNumber o == Nothing)) log
   table $ mapM_ viewLog (reverse ls)

viewLog :: Log -> Html
viewLog (Log _ t s) = do
   tr $ do
      td $ string $ formatTime defaultTimeLocale "%Y/%m/%d_%H:%M" t
      td $ p $ string s

newInput :: (TVar Session) -> EventNumber -> GameName -> RoutedNomyxServer Response
newInput ts en gn = toResponse <$> do
    pn <- getPlayerNumber ts
    s <- liftIO $ atomically $ readTVar ts
    let g = find ((== gn) . getL (game >>> gameName)) (_games $ _multi s)
    let eventHandler = fromJust $ findEvent en (_events $ _game $ fromJust g)
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" (getNomyxForm eventHandler)
    link <- showURL MainPage
    case r of
       (Right c) -> do
          webCommand ts $ M.inputResult pn en c gn
          seeOther link $ string "Redirecting..."
       (Left _) -> do
          liftIO $ putStrLn $ "cannot retrieve form data"
          seeOther link $ string "Redirecting..."

getNomyxForm :: EventHandler -> NomyxForm UInputData
getNomyxForm (EH _ _ (InputEv (Input _ _ iForm)) _ _) = inputForm iForm
getNomyxForm _ = error "Not an Input Event"

inputForm :: (Typeable a) => InputForm a -> NomyxForm UInputData
inputForm (Radio choices)    = URadioData    <$> inputRadio' (zip [0..] (snd <$> choices)) ((==) 0) <++ RB.label " "
inputForm Text               = UTextData     <$> RB.inputText "" <++ RB.label " "
inputForm TextArea           = UTextAreaData <$> RB.textarea 50 5  "" <++ RB.label " "
inputForm Button             = pure UButtonData
inputForm (Checkbox choices) = UCheckboxData <$> RB.inputCheckboxes (zip [0..] (snd <$> choices)) (const False) <++ RB.label " "

showHideTitle :: String -> Bool -> Bool -> Html -> Html -> Html
showHideTitle id visible empty title rest = do
   div ! A.onclick (fromString $ printf "toggle_visibility('%sBody', '%sShow')" id id) $ table ! A.width "100%" $ tr $ do
      td $ title ! A.width "80%"
      td ! A.style "text-align:right;" $ h5 (if visible then "[Click to hide]" else "[Click to show]") ! A.id (fromString $ printf "%sShow" id) ! A.width "20%"
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

delGame :: (TVar Session) -> GameName -> RoutedNomyxServer Response
delGame ts gn = do
   webCommand ts (M.delGame gn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

viewGamePlayer :: (TVar Session) -> GameName -> RoutedNomyxServer Response
viewGamePlayer ts gn = do
   pn <- getPlayerNumber ts
   webCommand ts (M.viewGamePlayer gn pn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."

titleWithHelpIcon :: Html -> String -> Html
titleWithHelpIcon title help = table ! A.width "100%" $ tr $ do
   td ! A.style "text-align:left;" $ title
   td ! A.style "text-align:right;" $ img ! A.src "/static/pictures/help.jpg" ! A.title (toValue help)
