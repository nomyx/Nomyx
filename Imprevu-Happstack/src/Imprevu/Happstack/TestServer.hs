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
   let (EvalEnv s' _) = execState ev (EvalEnv s defaultEvalConf)
   atomically $ writeTVar tvs s'

mainPage :: WebState -> ServerPartT IO Response
mainPage ws@(WebState tts _ _) = do
   (TestState eis os _) <- liftIO $ atomically $ readTVar tts
   let link en sa iv = pack $ "/test/do-input/" ++ (show en) ++ "/" ++ (show sa) ++ "/" ++ (urlEncode $ show iv)
   m <- mapM (viewInput 1 ws link) eis
   return $ toResponse $ do
     sequence_ $ catMaybes m
     toHtml $ show os

