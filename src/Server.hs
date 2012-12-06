-- | This server handles asynchronous text connections.
-- It has 4 threads:
-- mainLoop: dispatch messages between every threads (this is the main program thread, not forked)
-- acceptLoop: handles new client connections
-- multiLoop: handles game commands. It contains the whole game state.
-- clientLoop: one per client, handles clients communications.

-- And 5 channels:
-- acceptChan: used by acceptLoop to publish new clients connections to mainLoop.
-- gameChan: used for the communication between mainLoop and multiLoop.
-- clientChan: one per client, use by the clients to communicate with mainLoop.
-- handleFree: one per client, used for upstream comunication with clientLoops, to control usage of the Handle.
-- debugChan: used to pass debug commands

   
module Server where

{-
-- Module Network is the simple networking library, presenting a Handle-based interface.
import Network (listenOn, accept, sClose, Socket,
                withSocketsDo, PortID(..))

import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import Multi
import Control.Monad.State
import System.IO
import Data.Char
import Control.Applicative
import Data.List
import Network.BSD
import Language.Haskell.Interpreter.Server --TODO: hide in Interpret
import Control.Concurrent.Process hiding (Handle)
import Commands
import Data.Function (on)
import Language.Nomic.Expression

type ClientNumber = Int

-- | associate a player number with a handle
data PlayerClient = PlayerClient { cPlayerNumber :: PlayerNumber,
                                   cHandle :: Handle}
                                   deriving (Eq, Show)
   
-- | A structure to hold the active games and players
data Server = Server { playerClients :: [PlayerClient],
                       interpreterHandle :: ServerHandle}
                       --deriving (Eq)

-- | A State to pass around active games and players.
-- Furthermore, the output are to be made with Comm to output to the right console.
type ServerState = StateT Server IO ()
                       
-- the channels to pass commands
type CommandChan = TChan String

-- type of the channel to pass commands upstream to the client thread
--data ClientCommands = Ready | Quit
type ClientChan = TChan String
                                   
-- | communication between clients and server
data ClientComm = ClientComm {inChan :: TChan String,
                              outChan :: TChan String,
                              handle :: Handle}

-- | a channel where to publish new clients connection
type AcceptChan = TChan ClientComm
      
defaultServer :: ServerHandle -> Server
defaultServer = Server []


type DebugServer = (ServerState, TChan Server)

defaultDebugServer = do
   debugChan <- atomically newTChan
   return (debugViewState, debugChan)


-- | An helper function that makes it clear how to use the state transformer Server.
runWithServer :: Server -> ServerState -> IO ()
runWithServer = flip evalStateT


-- | a loop that will handle game commands
runMulti :: AcceptChan -> DebugServer -> ServerHandle -> IO ()
runMulti acceptChan debug sh = runWithServer (defaultServer sh) (mainLoop acceptChan [] debug)

   
-- | Start Nomic in server mode
serverStart :: PortNumber -> ServerHandle -> IO ()
serverStart port sh = withSocketsDo $ do
    servSock <- listenOn $ PortNumber port
    host <- getHostName
    putStrLn $ "Starting telnet server...\nTo connect, try \"telnet " ++ host ++ " " ++ (show port) ++ "\" in a shell window"
    startAll servSock sh `catch` (\_ -> putStrLn "serverStart: Closing") `finally` sClose servSock


-- | starts every threads
startAll :: Socket -> ServerHandle -> IO ()
startAll servSock sh = do
    -- Fork the loop that will handle new client connections along with its channel
    acceptChan <- atomically newTChan

    --start the loop that will  handle client's connections
    --forkIO $ acceptLoop servSock acceptChan
    spawn $ makeProcess id (acceptLoop servSock acceptChan)-- `catch` (\_ -> do putStrLn "acceptLoop: Closing"; return ())

    -- the multi loop will centralize and dispatch communications
    def <- defaultDebugServer
    runMulti acceptChan def sh

    


-- | the loop will handle new client connections and fork a subsequent thread for each client
acceptLoop :: Socket -> AcceptChan -> Process () ()
acceptLoop servSock acceptChan = do -- acceptChan
   (cHandle, _, _) <- lift $ accept servSock
   --hSetEcho cHandle True
   liftIO $ hSetBuffering cHandle NoBuffering
	
	-- Fork loops that will handle client communication
   cc <- liftIO $ newClientComm cHandle
   liftIO $ forkIO $ clientIn  cc `catch` (\_ -> putStrLn "acceptLoop: clientIn exception")
   liftIO $ forkIO $ clientOut cc `catch` (\_ -> putStrLn "acceptLoop: clientOut exception")
   
	-- publish new client connection with its chan and handle on acceptChan
   lift $ atomically $ writeTChan acceptChan cc
   acceptLoop servSock acceptChan


newClientComm :: Handle -> IO ClientComm
newClientComm h = do
   inChan <- liftIO $ atomically newTChan
   outChan <- liftIO $ atomically newTChan
   return ClientComm {inChan = inChan,
                      outChan = outChan,
                      handle = h}


-- | a loop that will handle client communication
clientIn :: ClientComm -> IO ()
clientIn cc = do
   s <- hGetLine $ handle cc
   atomically $ writeTChan (inChan cc) s
   clientIn cc

-- | a loop that will handle client communication
clientOut :: ClientComm -> IO ()
clientOut cc = do
   s <- atomically $ readTChan (outChan cc)
   hPutStr (handle cc) s
   clientOut cc



selectIn :: [ClientComm] -> STM (String, ClientComm)
selectIn l = do
   let f cc@ClientComm {inChan = ic} = do
        s <- readTChan ic
        return (s, cc)
   foldl orElse retry (map f l)

selectOut :: [ClientComm] -> STM (String, ClientComm)
selectOut l = do
   let f cc@ClientComm {outChan = oc} = do
        s <- readTChan oc
        return (s, cc)
   foldl orElse retry (map f l)

data ClientData = ClientAccept ClientComm
                | ClientInput (String, ClientComm)

-- | the server loop will dispatch messages between threads
mainLoop :: AcceptChan -> [ClientComm] -> DebugServer -> ServerState
mainLoop acceptChan clients d@(debugState, debugChan) = do

   --read on both acceptChan and all client's chans
   r <- lift $ atomically $ (ClientAccept `fmap` readTChan acceptChan)
                              `orElse`
                            (ClientInput `fmap` selectIn clients)

   case r of
      -- new data on the accept chan (CommandChan, ClientChan, Handle)
      ClientAccept ca -> do
         --register new client
         newClient ca
         --loop
         mainLoop acceptChan (ca:clients) d
               
      -- new data on the clients chan (String, ClientChan, Handle)
      ClientInput (l,cc) -> do  
         -- do some filtering
         let myLine = filter isPrint l
         --putStrLn $ "data: " ++ myLine ++ " from handle: " ++ show h
         case myLine of
            "debug read" -> do
               s <- get
               --write state into the channel (for external reading).
               lift $ atomically $ writeTChan debugChan s
            "debug write" -> do
               --execute the debug monad.
               debugState
            "quit" -> playerQuit (handle cc)
            ""     -> return ()
            -- every other command is passed through
            _      -> issuePlayerCommand myLine cc



   --loop
   mainLoop acceptChan clients d


newClient :: ClientComm -> ServerState
newClient cc = do
   (Server pcs sh) <- get
   let comm = (Communication (inChan cc) (outChan cc) sh)
   pn <- liftIO $ evalStateT newPlayer comm
   modify (\ser -> ser { playerClients = PlayerClient { cPlayerNumber = pn,
                                                        cHandle = (handle cc)} : pcs})



-- | issue the player's command with the right Comm and Multi environnement
issuePlayerCommand :: String -> ClientComm -> ServerState
issuePlayerCommand l cc = do
   (Server pcs sh) <- get
   let comm = (Communication (inChan cc) (outChan cc) sh)
   --issue the player's command 
   case getPlayerNumber (handle cc) pcs of
      Nothing -> error "issuePlayerCommand: player's handle not found"
      Just pn -> liftIO $ evalStateT (runLine l pn) comm


playerQuit :: Handle -> ServerState
playerQuit h = do
   pcs <- gets playerClients
   -- erase player client
   modify (\ser -> ser { playerClients = filter (\PlayerClient {cHandle = myh} -> myh /= h) pcs})

-- | gives the player's number associated to that handle
getPlayerNumber :: Handle -> [PlayerClient] -> Maybe PlayerNumber
getPlayerNumber h ps = cPlayerNumber <$> find (\PlayerClient {cHandle = myh} -> myh==h) ps

debugViewState :: ServerState
debugViewState = lift . print =<< get  

instance Ord PlayerClient where
   (<=) = (<=) `on`cPlayerNumber

instance Show Server where
   show Server{playerClients =pcs} = "\n" ++ (show $ sort pcs)
-}
