module FlakeIt.Cli.Parser where

import Data.Version (Version, showVersion)
import FlakeIt.DB qualified as DB
import FlakeIt.Template
import Options.Applicative

data Command
  = List !ListOptions
  | Add !AddOptions
  | Remove !RemoveOptions
  | Init !InitOptions
  | New !NewOptions
  deriving (Show)

cliParser :: Version -> ParserInfo Command
cliParser version = info (helper <*> versionP version <*> commandP) $ fullDesc <> progDesc "Flake It Program"

commandP :: Parser Command
commandP =
  hsubparser $
    mconcat
      [ command "add" (info addCommand $ progDesc "Add tempalates with flake url")
      , command "list" (info listCommand $ progDesc "List templates")
      , command "remove" (info removeCommand $ progDesc "Remove templates with flake url")
      , command "init" (info initCommand $ progDesc "Init flake template")
      , command "new" (info newCommand $ progDesc "New flake template")
      ]

templateCompleter :: Completer
templateCompleter = listIOCompleter completions
 where
  completions :: IO [String]
  completions = map templateToString . concatMap listTemplates <$> DB.getAll

urlCompleter :: Completer
urlCompleter = listIOCompleter completions
 where
  completions :: IO [String]
  completions = map (urlToString . (\s -> s.url)) <$> DB.getAll

versionP :: Version -> Parser (a -> a)
versionP version =
  infoOption flakeitVersion $
    long "version"
      <> short 'v'
      <> help "Show flakeit's version"
      <> hidden
 where
  flakeitVersion :: String
  flakeitVersion = "flakeit " <> "v" <> showVersion version

templateP :: Parser Template
templateP = strOption (long "template" <> short 't' <> help "Choose template to use" <> completer templateCompleter)

projectNameP :: Parser String
projectNameP = argument str (metavar "NAME")

urlP :: Parser SourceUrl
urlP = argument str (completer urlCompleter <> metavar "PATH")

data NewOptions = NewOptions
  { template :: !Template
  , projectName :: !String
  }
  deriving (Show)

newCommand :: Parser Command
newCommand = New <$> (NewOptions <$> templateP <*> projectNameP)

data InitOptions = InitOptions
  { template :: !Template
  }
  deriving (Show)

initCommand :: Parser Command
initCommand = Init . InitOptions <$> templateP

data RemoveOptions = RemoveOptions
  { url :: !SourceUrl
  }
  deriving (Show)

removeCommand :: Parser Command
removeCommand = Remove . RemoveOptions <$> urlP

data ListOptions = ListOptions
  {}
  deriving (Show)

listCommand :: Parser Command
listCommand = pure (List ListOptions)

data AddOptions = AddOptions
  { url :: SourceUrl
  }
  deriving (Show)

addCommand :: Parser Command
addCommand = Add . AddOptions <$> urlP
