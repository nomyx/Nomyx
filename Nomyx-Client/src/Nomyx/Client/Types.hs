{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Nomyx.Client.Types where

import           Options.Applicative
import           Data.Aeson
import           Language.Nomyx
import           GHC.Generics

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

data CmdLine = CmdLine { options :: Options,
                         comm    :: Command}
                         deriving (Show)

data Options = Options { verbose  :: Bool,
                         version  :: Bool,
                         test     :: Bool,
                         hostname :: String,
                         port     :: String}
                         deriving (Show)

data TemplateView = TemplateView { name         :: RuleName,         -- short name of the rule
                                   desc         :: String,           -- description of the rule
                                   code         :: RuleCode,         -- code of the rule as a string
                                   author       :: String,           -- the name of the original author
                                   picture      :: Maybe FilePath,   -- a file name for the illustration image
                                   category     :: [String],         -- categories
                                   declarations :: [FilePath]}       -- addictional declarations (Haskell modules)
                                   deriving (Generic, Show)

instance ToJSON TemplateView where
--    toEncoding = genericToEncoding defaultOptions

instance FromJSON TemplateView
