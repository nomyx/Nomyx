
> module Imprevu.Event where
>
> -- EventInfoN holds all infos on an active event
> data EventInfoN n = forall a. (Typeable a, Show a) =>
>    EventInfo {_eventNumber :: EventNumber,
>               event        :: EventM n a,
>               handler      :: (EventNumber, a) -> n (),
>               _evStatus    :: Status,
>               _env         :: [SignalOccurence]}

> module Nomyx.Core.Engine.Types
>
> -- | Environment necessary for the evaluation of any nomyx expressions or events
> data EvalState = EvalState { _eGame :: Game,             -- game to be read/modified
>                              _eRuleNumber :: RuleNumber} -- number of the rule requesting the evaluation
>
> type EvalEnv = EvalEnvN Nomex EvalState
>
> -- | Environment necessary for the evaluation of Nomex
> type Evaluate a = EvaluateN Nomex EvalState a
>
> -- | The state of the game:
> data Game = Game { _gameName    :: GameName,
>                    _gameDesc    :: GameDesc,
>                    _rules       :: [RuleInfo],
>                    _players     :: [PlayerInfo],
>                    _variables   :: [Var],
>                    _events      :: [RuleEventInfo],
>                    _outputs     :: [Output],
>                    _victory     :: Maybe VictoryInfo,
>                    _logs        :: [Log],
>                    _currentTime :: UTCTime,
>                    _randomGen   :: StdGen}
>                    deriving (Typeable)
>
> data RuleEventInfo = RuleEventInfo { _erRuleNumber :: RuleNumber,  -- the rule that created the event
>                                      _erEventInfo :: EventInfo}    -- event informations
>                                      deriving (Eq)


> module Nomyx.Core.Types
>
> -- | Session contains all the game informations.
> data Session = Session { _sh           :: ServerHandle,
>                          _multi        :: Multi,
>                          _acidProfiles :: AcidState ProfileDataState}
>
> -- | A structure to hold the active games and players
> data Multi = Multi { _gameInfos :: [GameInfo],
>                      _mSettings :: Settings,
>                      _mLibrary  :: Library}
>                      deriving (Eq, Show, Typeable)
>
> -- | Informations on a particular game
> data GameInfo = GameInfo { _loggedGame     :: LoggedGame,
>                            _ownedBy        :: Maybe PlayerNumber,
>                            _forkedFromGame :: Maybe GameName,
>                            _isPublic       :: Bool,
>                            _startedAt      :: UTCTime}
>                            deriving (Typeable, Show, Eq)



> module Imprevu.Evaluation.EventEval
>
> class HasEvents n s where
>    getEvents :: s -> [EventInfoN n]
>    setEvents :: [EventInfoN n] -> s -> s
>
> -- | Environment necessary for the evaluation of events
> data EvalEnvN n s = EvalEnv { _evalEnv     :: s,
>                              evalFunc     :: forall a. n a -> EvaluateN n s a,           -- evaluation function
>                              errorHandler :: EventNumber -> String -> EvaluateN n s ()}  -- error function
>
> -- | Environment necessary for the evaluation of Nome
> type EvaluateN n s a = ExceptT String (State (EvalEnvN n s)) a



> module Imprevu.Happstack.Types
>
> data InputResult = InputResult EventNumber SignalAddress InputView InputDataView
>
> data WebStateN n s = WebState {_webState     :: TVar s,
>                                updateSession :: TVar s -> InputResult -> IO (),               -- update the session after an input is submitted
>                                evalFunc      :: forall a. n a -> EvaluateN n s a,             -- evaluation function used to compute the remaining signals/inputs and display them
>                                errorHandler  :: EventNumber -> String -> EvaluateN n s ()}    -- error function



> module Nomyx.Web.Types where
>
> data WebSession = WebSession {_webSession        :: WebStateN Nomex Session,
>                               _authState         :: AuthState}
>
> type RoutedNomyxServer a = RouteT PlayerCommand (StateT WebSession (ServerPartT IO)) a


> module Nomyx.Core.Engine.Evaluation where
>
> evalNomex :: Nomex a -> Evaluate a
>
> runEvalError :: RuleNumber -> Maybe PlayerNumber -> Evaluate a -> State Game ()
> runEvalError rn mpn eva = do --error "runEvalError
>   g <- get
>   let (EvalEnv (EvalState g' _) _ _) = execState (Imp.runEvalError' eva) (EvalEnv (EvalState g rn) evalNomex undefined)
>   put g'
>
> runSystemEval :: PlayerNumber -> Evaluate a -> State Game ()
> runSystemEval pn = runEvalError 0 (Just pn)
