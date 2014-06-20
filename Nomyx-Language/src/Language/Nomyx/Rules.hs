{-# LANGUAGE ScopedTypeVariables #-}

-- | Basic rules building blocks.
-- for example, you can suppress rule 1 with:
--do
--   suppressRule 1

module Language.Nomyx.Rules (
   RuleNumber,
   RuleCode,
   RuleEvent(..),
   RuleStatus(..),
   MetaRule,
   activateRule, activateRule_,
   rejectRule, rejectRule_,
   getRules, getActiveRules, getRule,
   getRulesByNumbers,
   getRuleFuncs,
   addRule, addRule_, addRuleParams,
   getFreeRuleNumber,
   suppressRule, suppressRule_, suppressAllRules,
   modifyRule,
   autoActivate,
   activateOrRejectRule,
   simulate,
   metaruleVar, createMetaruleVar, addMetarule, testWithMetaRules, displayMetarules,
   legal, illegal, noPlayPlayer, immutableRule,
   autoDelete,
   eraseAllRules,
   getSelfRuleNumber, getSelfRule,
   showRule
   ) where

import Prelude hiding (foldr)
import Language.Nomyx.Expression
import Language.Nomyx.Events
import Language.Nomyx.Variables
import Language.Nomyx.Outputs
import Data.Lens
import Control.Monad
import Data.List
import Data.Maybe
import Control.Applicative

-- * Rule management

-- | activate a rule: change its state to Active and execute it
activateRule :: RuleNumber -> Nomex Bool
activateRule = ActivateRule

activateRule_ :: RuleNumber -> Nomex ()
activateRule_ r = activateRule r >> return ()

-- | reject a rule: change its state to Suppressed and suppresses all its environment (events, variables, inputs)
-- the rule can be activated again later
rejectRule :: RuleNumber -> Nomex Bool
rejectRule = RejectRule

rejectRule_ :: RuleNumber -> Nomex ()
rejectRule_ r = void $ rejectRule r

getRules :: NomexNE [RuleInfo]
getRules = GetRules

getActiveRules :: NomexNE [RuleInfo]
getActiveRules = filter ((== Active) . _rStatus) <$> getRules

getRule :: RuleNumber -> NomexNE (Maybe RuleInfo)
getRule rn = do
   rs <- GetRules
   return $ find ((== rn) . getL rNumber) rs

getRulesByNumbers :: [RuleNumber] -> NomexNE [RuleInfo]
getRulesByNumbers rns = mapMaybeM getRule rns

getRuleFuncs :: NomexNE [Nomex ()]
getRuleFuncs = map _rRule <$> getRules

-- | add a rule to the game, it will have to be activated
addRule :: RuleInfo -> Nomex Bool
addRule r = AddRule r

addRule_ :: RuleInfo -> Nomex ()
addRule_ r = void $ AddRule r

--TODO: too permissive. Should use SubmitRule instead.
addRuleParams :: RuleName -> Rule -> RuleCode -> String -> Nomex RuleNumber
addRuleParams name rule code desc = do
   number <- liftEffect getFreeRuleNumber
   res <- addRule $ defaultRule {_rName = name, _rRule = rule, _rRuleCode = code, _rNumber = number, _rDescription = desc}
   return $ if res then number else error "addRuleParams: cannot add rule"


getFreeRuleNumber :: NomexNE RuleNumber
getFreeRuleNumber = getFreeNumber . map _rNumber <$> getRules

getFreeNumber :: (Eq a, Num a, Enum a) => [a] -> a
getFreeNumber l = head [a| a <- [1..], not $ a `elem` l]

--suppresses completly a rule and its environment from the system
suppressRule :: RuleNumber -> Nomex Bool
suppressRule rn = RejectRule rn

suppressRule_ :: RuleNumber -> Nomex ()
suppressRule_ rn = void $ RejectRule rn

suppressAllRules :: Nomex Bool
suppressAllRules = do
    rs <- liftEffect getRules
    res <- mapM (suppressRule . _rNumber) rs
    return $ and res

modifyRule :: RuleNumber -> RuleInfo -> Nomex Bool
modifyRule rn r = ModifyRule rn r

-- | allows a rule to retrieve its own number (for auto-deleting for example)
getSelfRuleNumber :: NomexNE RuleNumber
getSelfRuleNumber = SelfRuleNumber

getSelfRule :: NomexNE RuleInfo
getSelfRule  = do
   srn <- getSelfRuleNumber
   rs:[] <- getRulesByNumbers [srn]
   return rs

-- | activate or reject a rule
activateOrRejectRule :: RuleInfo -> Bool -> Nomex ()
activateOrRejectRule r b = if b then activateRule_ (_rNumber r) else rejectRule_ (_rNumber r)

-- | a rule can autodelete itself (generaly after having performed some actions)
autoDelete :: Nomex ()
autoDelete = liftEffect getSelfRuleNumber >>= suppressRule_

-- | All rules from player p are erased:
eraseAllRules :: PlayerNumber -> Nomex Bool
eraseAllRules p = do
    rs <- liftEffect $ getRules
    let myrs = filter ((== p) . getL rProposedBy) rs
    res <- mapM (suppressRule . _rNumber) myrs
    return $ and res

-- | This rule will activate automatically any new rule.
autoActivate :: Nomex ()
autoActivate = void $ onEvent_ (ruleEvent Proposed) (activateRule_ . _rNumber)

-- * Meta Rules

-- | A meta rule is a rule that can juge the legality of another rule.
type MetaRule = RuleInfo -> NomexNE Bool

-- | The meta rules are stored in a list variable
metaruleVar :: MsgVar [(String, MetaRule)]
metaruleVar = msgVar "metarules"

-- | create the meta rule variable
createMetaruleVar :: Nomex ()
createMetaruleVar = void $ newMsgVar' metaruleVar []

-- | add a new metarule to the list
addMetarule :: MetaRule -> String -> Nomex ()
addMetarule mr code = void $ modifyMsgVar metaruleVar ((code, mr):)

-- | use the list of meta rules to juge a new rule
testWithMetaRules :: RuleInfo -> NomexNE Bool
testWithMetaRules r = do
   mmrs <- readMsgVar metaruleVar
   case mmrs of
      Just mrs -> and <$> mapM (($r) . snd) mrs
      Nothing ->  return False

displayMetarules :: Nomex ()
displayMetarules = void $ displayVar Nothing metaruleVar dispAll where
   dispAll mvs = return $ maybe "No meta rules" (("Meta Rules:\n" ++) . concatMap disp) mvs
   disp (s, _) = s ++ "\n"

-- | A rule will be always legal
legal :: MetaRule
legal = const $ return True

-- | A rule will be always illegal
illegal :: MetaRule
illegal = const $ return False


-- | Player p cannot propose any more rules
noPlayPlayer :: PlayerNumber -> MetaRule
noPlayPlayer pn rule = return $ (_rProposedBy rule) /= pn

-- | rule number rn cannot be deleted by any incoming rule
-- we simulate the execution of an incoming rule to make sure it doesn't delete the immutable rule
immutableRule :: RuleNumber -> MetaRule
immutableRule rn = \rule -> do
   immu <- getRule rn
   maybe (return True) (const $ simulate (_rRule rule) (isJust <$> getRule rn)) immu

-- | simulate the execution of rule "sim" and then run rule "test" over the result
simulate :: Nomex a -> NomexNE Bool -> NomexNE Bool
simulate sim test = Simu sim test



-- | a default rule
defaultRule = RuleInfo  {
    _rNumber       = 1,
    _rName         = "",
    _rDescription  = "",
    _rProposedBy   = 0,
    _rRuleCode     = "",
    _rRule         = return (),
    _rStatus       = Pending,
    _rAssessedBy   = Nothing}

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f

showRule x = void $ NewOutput Nothing (return $ show x)
