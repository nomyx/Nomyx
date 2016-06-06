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

