

module Imprevu.Happstack.Test where


import Imprevu.Happstack.Forms
import Imprevu.Happstack.Types
import Data.Maybe

routedCommands :: Command -> RoutedServer Response
routedCommands Main             = nomyxPage
routedCommands (DoInput en fa ft) = newInput en fa ft

nomyxPage :: RoutedServer Response
nomyxPage = do
   m <- viewInput
   return $ toResponse $ fromJust m

nomyxSite :: Site Command (ServerPartT IO Response)
nomyxSite = setDefault Main $ mkSitePI $ (\a b -> evalStateT (runRouteT (catchRouteError . routedCommands) a b) ws)


launchWebServer :: TVar Session -> IO ()
launchWebServer ts net = do
   putStrLn $ "Starting web server..."
   s <- liftIO $ atomically $ readTVar ts
   let set = _mSettings $ _multi s
   let conf = nullConf {HS.port = 80}
   simpleHTTP conf $ server set net docdir

--serving Nomyx web page as well as data from this package and the language library package
server :: WebState -> Settings -> Network -> String -> ServerPartT IO Response
server ws set net docdir = mconcat [
    do decodeBody (defaultBodyPolicy "/tmp/" 102400 4096 4096)
       html <- implSite (pack $ nomyxURL net) "/Nomyx" (nomyxSite ws)
       return $ toResponse html]

