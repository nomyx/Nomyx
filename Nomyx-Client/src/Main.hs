
module Main where

import           Nomyx.Client.Client
import           System.Environment
import           Options.Applicative

main :: IO ()
main = do
   args <- getArgs
   execParser optionsInfos >>= options
   --uploadTemplates $ head args

options :: Options -> IO ()
options os = putStrLn $ show os

data Command = Player
             | Game
             | Rule
             | Action
             | Template TemplateCom
             deriving (Show)

data TemplateCom = Add
                 | Replace FilePath
                 | Get
                 deriving (Show)

data Options = Options
  { verbose  :: Bool,
    version  :: Bool,
    test     :: Bool,
    hostname :: String,
    port     :: String,
    comm     :: Command}
    deriving (Show)

optionsInfos :: ParserInfo Options
optionsInfos = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Welcome to Nomyx!"
     <> header "a client for the Nomyx game" )

optionsParser :: Parser Options
optionsParser = Options
     <$> switch    ( long "verbose" <> help "Verbose ouput" )
     <*> switch    ( long "version" <> help "Show version number" )
     <*> switch    ( long "test"    <> help "Perform routine tests" )
     <*> strOption ( long "hostname" <> value "localhost" <> metavar "TARGET" <> help "Hostname of the Nomyx server (default is localhost)" )
     <*> strOption ( long "port"     <> value "8000"      <> metavar "TARGET" <> help "port of the Nomyx server (default is 8000)" )
     <*> commandParser

commandParser :: Parser Command
commandParser = subparser
  ( command "player" (info (pure Player) ( progDesc "manage players" ))
  <> command "game" (info (pure Game) ( progDesc "manage games" ))
  <> command "rule" (info (pure Rule) ( progDesc "manage rules" ))
  <> command "action" (info (pure Action) ( progDesc "manage actions" ))
  <> command "template" (info (templateParser) ( progDesc "manage templates" ))
 )

templateParser :: Parser Command
templateParser = Template <$> subparser
  ( command "add" (info (pure Add) ( progDesc "add a template" ))
  <> command "replace" (info (Replace <$> (argument str (metavar "TARGET..."))) ( progDesc "replace all temaplates" ))
  <> command "get" (info (pure Get) ( progDesc "get all templates" ))
 )



-- | Launch mode
data Flag = Verbose
          | Version
          | Test
          | HostName String
          | Port String
          | Help
       deriving (Show, Eq)

-- | launch options description
--options :: [OptDescr Flag]
--options =
--     [ Option "v" ["verbose"]   (NoArg Verbose)                "chatty output on stderr"
--     , Option "V" ["version"]   (NoArg Version)                "show version number"
--     , Option "h" ["host"]      (ReqArg HostName "Hostname")   "specify server host name"
--     , Option "p" ["port"]      (ReqArg Port "Port")           "specify server port"
--     , Option "t" ["tests"]     (NoArg Test)                   "perform routine check"
--     , Option "?" ["help"]      (NoArg Help)                   "display usage options (this screen)"
--     ]
--
--nomyxOpts :: [String] -> IO ([Flag], [String])
--nomyxOpts argv =
--       case getOpt Permute options argv of
--          (o,n,[]  ) -> return (o,n)
--          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
--
--header :: String
--header = "Usage: Nomyx [OPTION...]"
--
--findPort, findHost :: [Flag] -> Maybe String
--findPort      fs = listToMaybe [a | Port      a <- fs]
--findHost      fs = listToMaybe [a | HostName  a <- fs]
