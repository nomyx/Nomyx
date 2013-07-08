
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
import Debug.Trace (trace)
import Data.List.Split
import Data.Typeable
default (Integer, Double, Data.Text.Text)

viewGame :: Game -> PlayerNumber -> (Maybe LastRule) -> RoutedNomyxServer Html
viewGame g pn mlr = do
   let inGame = isJust $ Utils.getPlayer g pn
   rf <- viewRuleForm mlr inGame
   vios <- viewIOs pn (_events g) (_outputs g)
   ok $ table $ do
      tr $ td $ div ! A.id "gameDesc" $ viewGameDesc g
      tr $ td $ div ! A.id "rules" $ viewAllRules g
      tr $ td $ div ! A.id "ios" $ vios
      tr $ td $ div ! A.id "newRule" $ rf
      tr $ td $ div ! A.id "details" ! A.title (toValue Help.events)  $ viewDetails pn g

viewGameDesc :: Game -> Html
viewGameDesc g = do
   p $ h3 $ string $ "Viewing game: " ++ _gameName g
   p $ h4 $ "Description:" >> br >> string (_desc $ _gameDesc g)
   p $ h4 $ a "Agora" ! (A.href $ toValue (_agora $ _gameDesc g))
   p $ h4 $ "Players in game:"
   viewPlayers $ _players g
   p $ viewVictory g

viewPlayers :: [PlayerInfo] -> Html
viewPlayers pis = do
   let plChunks = transpose $ chunksOf (1 + (length pis) `P.div` 3) (sort pis)
   table $ mapM_ (\row -> tr $ mapM_ viewPlayer row) plChunks


viewPlayer :: PlayerInfo -> Html
viewPlayer pi = do
    td $ string $ show $ _playerNumber pi
    td $ string $ _playerName pi

viewVictory :: Game -> Html
viewVictory g = do
    let vs = _playerName <$> mapMaybe (Utils.getPlayer g) (_victory g)
    case vs of
        []   -> br
        a:[] -> h3 $ string $ "Player " ++ (show a) ++ " won the game!"
        a:bs -> h3 $ string $ "Players " ++ (concat $ intersperse ", " $ bs) ++ " and " ++ a ++ " won the game!"

viewAllRules :: Game -> Html
viewAllRules g = do
   h3 "Rules"
   viewRules (activeRules g)   "Active rules"     True g ! (A.title $ toValue Help.actives) >> br
   viewRules (pendingRules g)  "Pending rules"    True g ! (A.title $ toValue Help.pendings) >> br
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
   let pl = fromMaybe (show $ _rProposedBy nr) (_playerName <$> (Utils.getPlayer g $ _rProposedBy nr))
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
            code

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

viewDetails :: PlayerNumber -> Game -> Html
viewDetails pn g = showHideTitle "Details" False False (h3 "Details") $ do
   p $ h4 "Vars:"
   viewVars   (_variables g)
   p $ h4 "Events:"
   viewEvents (_events g)
   p $ h4 "Log:"
   viewLog    (_log g) pn

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

viewIOs :: PlayerNumber -> [EventHandler] -> [Output] -> RoutedNomyxServer Html
viewIOs pn ehs os = do
   vis <- viewInputs pn ehs
   let vos = viewOutput os pn
   ok $ do
      h3 "Rules Inputs/Ouputs"
      showHideTitle "Inputs" True False (h4 "Inputs:")  $ vis ! A.title (toValue Help.inputs)
      showHideTitle "Ouputs" True False (h4 "Outputs:") $ vos ! A.title (toValue Help.outputs)


viewInputs :: PlayerNumber -> [EventHandler] -> RoutedNomyxServer Html
viewInputs pn ehs = do
   mis <- mapM (viewInput pn) $ sort ehs
   let is = catMaybes mis
   ok $ table $ mconcat is

viewInput :: PlayerNumber -> EventHandler -> RoutedNomyxServer (Maybe Html)
viewInput me (EH eventNumber _ (InputEv (Input pn title iForm)) _ SActive) | me == pn = do
    link <- showURL (DoInput eventNumber)
    lf  <- lift $ viewForm "user" $ inputForm iForm
    return $ Just $ tr $ td $ do
       string title
       blazeForm lf (link) ! A.id "InputForm"
viewInput _ _ = return Nothing

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


newRuleForm :: (Maybe SubmitRule) -> NomyxForm SubmitRule
newRuleForm (Just lr) = newRuleForm' lr
newRuleForm Nothing = newRuleForm' (SubmitRule "" "" "")

newRuleForm' :: SubmitRule -> NomyxForm SubmitRule
newRuleForm' (SubmitRule name desc code) =
   pure SubmitRule  <*> RB.label "Name: " ++> (RB.inputText name)
                    <*> RB.label "      Short description: " ++> RB.inputText desc
                    <*> RB.label "      Code: " ++> RB.textarea 80 15 code
                           `RBC.setAttr` A.class_ "code" `RBC.setAttr` A.placeholder "Enter here your rule" `RBC.setAttr` (A.title (toValue Help.code))

viewRuleForm :: Maybe LastRule -> Bool -> RoutedNomyxServer Html
viewRuleForm msr inGame = do
   link <- showURL NewRule
   lf  <- lift $ viewForm "user" (newRuleForm (fst <$> msr))
   ok $ do
      h3 "Propose a new rule:"
      if inGame then do
         blazeForm lf (link)
         let error = snd <$> msr
         when (isJust error) $ do
            h5 $ "Error in submitted rule: "
            pre $ string $ fromJust error
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
             when (length rs' > length rs) $ trace "new rule mail " $ sendMailsNewRule s' sr pn
       (Left _) -> liftIO $ putStrLn $ "cannot retrieve form data"
   seeOther link $ string "Redirecting..."


viewOutput :: [Output] -> PlayerNumber -> Html
viewOutput os pn = do
   let myos = map _output $ filter (\(Output _ mypn _ _) -> mypn == pn) os
   mapM_ viewMessages [myos]

viewMessages :: [String] -> Html
viewMessages = mapM_ (\s -> pre $ string s >> br)

viewLog :: [Log] -> PlayerNumber -> Html
viewLog log pn = do
   let mylog = map snd $ filter (\o -> (fst o == Just pn) || (fst o == Nothing)) log
   pre $ mapM_ (\s -> p $ string s >> br) mylog

newInput :: EventNumber -> (TVar Session) -> RoutedNomyxServer Html
newInput en ts = do
    pn <- getPlayerNumber ts
    s <- liftIO $ atomically $ readTVar ts
    mgn <- liftIO $ getPlayersGame pn s
    let eventHandler = fromJust $ findEvent en (_events $ _game $ fromJust mgn)
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" (getNomyxForm eventHandler)
    link <- showURL MainPage
    case r of
       (Right c) -> do
          webCommand ts $ M.inputResult pn en c
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

viewGamePlayer :: (TVar Session) -> GameName -> RoutedNomyxServer Response
viewGamePlayer ts gn = do
   pn <- getPlayerNumber ts
   webCommand ts (M.viewGamePlayer gn pn)
   link <- showURL MainPage
   seeOther link $ toResponse "Redirecting..."
