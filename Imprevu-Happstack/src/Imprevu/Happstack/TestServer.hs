{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Imprevu.Happstack.TestServer where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Text
import Data.Typeable
import Happstack.Server            as HS
import Imprevu.Happstack.Forms
import Imprevu.Happstack.Types
import Imprevu.Evaluation
import Imprevu.Test.Test
import Imprevu.Test.TestMgt
import Text.Blaze.Html5            (toHtml)
import Imprevu
import Control.Applicative
import Network.HTTP (urlEncode)

type WebState = WebStateN TestM TestState


startTest :: TestM () -> IO ()
startTest t = do
  let ts = execSignals t [] defaultEvalEnv
  tv <- atomically $ newTVar ts
  launchWebServer tv


launchWebServer :: TVar TestState -> IO ()
launchWebServer tv = do
   putStrLn $ "Starting web server on http://localhost:8080/test/main"
   let conf = nullConf {HS.port = 8080}
   let ws = WebState tv updateSessionTest defaultEvalConf
   forkIO $ launchTimeEvents tv defaultEvalConf
   simpleHTTP conf $ server ws

--serving Nomyx web page as well as data from this package and the language library package
server :: WebState -> ServerPartT IO Response
server ws = do
  decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
  msum [dirs "test/main" (mainPage ws),
                  dirs "test/do-input" $
                      path $ \en ->
                      path $ \is ->
                      newInput is en ws "/test/main"
                 ]


updateSessionTest :: TVar TestState -> InputS -> InputData -> EventNumber -> IO ()
updateSessionTest tvs is id _ = do
   s <- atomically $ readTVar tvs
   putStrLn $ show s
   putStrLn  $ "input result: Form " ++ show is ++ ", choice " ++ show id
   let ev = runEvalError' $ triggerInput is id
   let (EvalEnv s' _) = execState ev (EvalEnv s defaultEvalConf)
   atomically $ writeTVar tvs s'

mainPage :: WebState -> ServerPartT IO Response
mainPage ws@(WebState tts _ _) = do
   (TestState eis os _) <- liftIO $ atomically $ readTVar tts
   let link en iv = pack $ "/test/do-input/" ++ (urlEncode $ show en) ++ "/" ++ (urlEncode $ show iv)
   m1 <- mapM (viewInput 1 ws link) eis
   m2 <- mapM (viewInput 2 ws link) eis
   m3 <- mapM (viewInput 3 ws link) eis
   m4 <- mapM (viewInput 4 ws link) eis
   m5 <- mapM (viewInput 5 ws link) eis
   return $ toResponse $ do
     "Test simple input:"
     sequence_ $ catMaybes m1
     "Test sum of events (first input wins):"
     sequence_ $ catMaybes m2
     "Test product of events (both inputs are necessary):"
     sequence_ $ catMaybes m3
     "Test monadic events (enter \"coco\" to get a second input)"
     sequence_ $ catMaybes m4
     "Test radio events:"
     sequence_ $ catMaybes m5
     "Results:\n"
     toHtml $ show os

