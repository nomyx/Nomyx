
module Main where

import           Nomyx.Client.Client
import           Nomyx.Client.Types
import           System.Environment
import           Options.Applicative

main :: IO ()
main = do
   (CmdLine os com) <- execParser optionsInfos
   putStrLn $ show os
   case com of
      Template (Replace f) -> uploadTemplates f os


optionsInfos :: ParserInfo CmdLine
optionsInfos = info (helper <*> (CmdLine <$> optionsParser <*> commandParser))
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
  <> command "replace" (info (Replace <$> (argument str (metavar "TARGET..."))) ( progDesc "replace all templates" ))
  <> command "get" (info (pure Get) ( progDesc "get all templates" ))
 )


