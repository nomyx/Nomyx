{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Imprevu.Happstack.Test where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Text
import Happstack.Server            as HS
import Imprevu.Happstack.Forms
import Imprevu.Happstack.Types
import Imprevu.Evaluation.InputEval
import Imprevu.Evaluation.EventEval
import Imprevu.Evaluation.TimeEval
import Imprevu.Test.Test
import Imprevu.Test.TestMgt
import Text.Blaze.Html5            (toHtml)
import Imprevu.Events
import Imprevu.Inputs
import Control.Applicative

type WebState = WebStateN TestM TestState


start :: IO ()
start = do
  let ts = execSignals (testSingleInput >> testMultipleInputs) [] defaultEvalEnv
  --let ts = execSignals testSome [] defaultEvalEnv
  tv <- atomically $ newTVar ts
  launchWebServer tv

testSome :: TestM ()
testSome = void $ onEvent_ (some $ inputText 1 "Enter text") f where
   f a = putStrLn' $ show a

launchWebServer :: TVar TestState -> IO ()
launchWebServer tv = do
   putStrLn $ "Starting web server..."
   let conf = nullConf {HS.port = 8080}
   let ws = WebState tv updateSessionTest evalEvents undefined
   forkIO $ launchTimeEvents tv (EvalFunc evalEvents undefined)
   simpleHTTP conf $ server ws

--serving Nomyx web page as well as data from this package and the language library package
server :: WebState -> ServerPartT IO Response
server ws = do
  decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
  msum [dirs "test/main" (mainPage ws),
                  dirs "test/do-input" $
                      path $ \en ->
                      path $ \sa ->
                      path $ \iv ->
                      newInput en sa iv ws "/test/main"
                 ]

updateSessionTest :: TVar TestState -> InputResult -> IO ()
updateSessionTest tvs (InputResult en sa ff ida) = do
   s <- atomically $ readTVar tvs
   putStrLn $ show s
   putStrLn  $ "input result: EventNumber " ++ show en ++ ", SignalAddress " ++ show sa ++ ", Form " ++ show ff ++ ", choice " ++ show ida
   let ev = runEvalError' $ triggerInput ff ida sa en
   let (EvalEnv s' _ _) = execState ev (EvalEnv s evalEvents undefined)
   atomically $ writeTVar tvs s'

mainPage :: WebState -> ServerPartT IO Response
mainPage ws@(WebState tts _ _ _) = do
   (TestState eis os _) <- liftIO $ atomically $ readTVar tts
   let link en sa iv = pack $ "/test/do-input/" ++ (show en) ++ "/" ++ (show sa) ++ "/" ++ (show iv)
   m <- mapM (viewInput 1 ws link) eis
   return $ toResponse $ do
     sequence_ $ catMaybes m
     toHtml $ show os
