
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
      Template (Put f) -> uploadLibrary f os
      Template (Get f) -> downloadLibrary f os


optionsInfos :: ParserInfo CmdLine
optionsInfos = info (helper <*> (CmdLine <$> optionsParser <*> commandParser))
       (fullDesc
     <> progDesc "This program allows you to manage your rules in Nomyx."
     <> header "a client for the Nomyx game")

optionsParser :: Parser Options
optionsParser = Options
     <$> switch    ( long "verbose"                                           <> help "Verbose ouput")
     <*> switch    ( long "version"                                           <> help "Show version number")
     <*> switch    ( long "test"                                              <> help "Perform routine tests")
     <*> strOption ( long "hostname" <> value "localhost" <> metavar "TARGET" <> help "Hostname of the Nomyx server (default is localhost)")
     <*> strOption ( long "port"     <> value "8000"      <> metavar "TARGET" <> help "port of the Nomyx server (default is 8000)")

commandParser :: Parser Command
commandParser = subparser
    (command "templates" (info (templateParser) (progDesc "manage templates. \n Subcommands: get, put."))
  <> command "player"    (info (pure Player)    (progDesc "manage players"))
  <> command "game"      (info (pure Game)      (progDesc "manage games"))
  <> command "rule"      (info (pure Rule)      (progDesc "manage rules"))
  <> command "action"    (info (pure Action)    (progDesc "manage actions")))

templateParser :: Parser Command
templateParser = Template <$> subparser
    (command "put" (info (Put <$> (argument str (metavar "<templates.yaml path>"))) (progDesc "replace all templates"))
  <> command "get" (info (Get <$> (argument str (metavar "<templates.yaml path>"))) (progDesc "get all templates")))


