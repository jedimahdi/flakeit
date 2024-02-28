module FlakeIt.Cli.Parser where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (Version, showVersion)
import FlakeIt.DB qualified as DB
import FlakeIt.Nix qualified as Nix
import FlakeIt.Template
import FlakeIt.Types
import Options.Applicative

cliParser :: Version -> ParserInfo Command
cliParser version = info (helper <*> versionP version <*> commandP) $ fullDesc <> progDesc "Flake It Program"

versionP :: Version -> Parser (a -> a)
versionP version =
  infoOption (flakeitVersion version) $
    long "version"
      <> short 'v'
      <> help "Show flakeit's version"
      <> hidden

flakeitVersion :: Version -> String
flakeitVersion version = "flakeit " <> "v" <> showVersion version

templateCompleter :: Completer
templateCompleter = mkCompleter comp
 where
  comp :: String -> IO [String]
  comp s = do
    map Text.unpack . concatMap listTemplates <$> DB.getAll

pathCompleter :: Completer
pathCompleter = mkCompleter comp
 where
  comp :: String -> IO [String]
  comp s =
    filter (\t -> s `List.isPrefixOf` t) . map (Text.unpack . (\l -> l.url)) <$> DB.getAll

data Command
  = List !ListOptions
  | Add !AddOptions
  | Remove !RemoveOptions
  | Update !UpdateOptions
  | Init !InitOptions
  | New !NewOptions
  deriving (Show)

data NewOptions = NewOptions
  { template :: !Text
  , name :: !Text
  }
  deriving (Show)

newOpts :: Parser NewOptions
newOpts =
  NewOptions
    <$> strOption (long "template" <> short 't' <> help "Choose template to use" <> completer templateCompleter)
    <*> argument str (completer pathCompleter <> metavar "NAME")

newCommand :: Parser Command
newCommand =
  New
    <$> ( NewOptions
            <$> strOption (long "template" <> short 't' <> help "Choose template to use" <> completer templateCompleter)
            <*> argument str (completer pathCompleter <> metavar "NAME")
        )

data InitOptions = InitOptions
  { template :: !Text
  }
  deriving (Show)

initCommand :: Parser Command
initCommand =
  Init . InitOptions
    <$> strOption (long "template" <> short 't' <> help "Choose template to use" <> completer templateCompleter)

data UpdateOptions = UpdateOptions
  { path :: Text
  }
  deriving (Show)

updateOpts :: Parser UpdateOptions
updateOpts =
  UpdateOptions
    <$> argument str (completer pathCompleter <> metavar "PATH")

updateCommand :: Parser Command
updateCommand =
  Update . UpdateOptions
    <$> argument str (completer pathCompleter <> metavar "PATH")

data RemoveOptions = RemoveOptions
  { path :: Text
  }
  deriving (Show)

removeCommand :: Parser Command
removeCommand =
  Remove . RemoveOptions
    <$> argument str (completer pathCompleter <> metavar "PATH")

data ListOptions = ListOptions
  {}
  deriving (Show)

listCommand :: Parser Command
listCommand = pure (List ListOptions)

data AddOptions = AddOptions
  { path :: Text
  }
  deriving (Show)

addCommand :: Parser Command
addCommand = Add . AddOptions <$> argument str (metavar "PATH")

commandP :: Parser Command
commandP =
  hsubparser $
    mconcat
      [ command "add" (info addCommand $ progDesc "Add a flake tempalate url")
      , command "list" (info listCommand $ progDesc "List templates")
      , command "remove" (info removeCommand $ progDesc "Remove flake template url")
      , command "update" (info updateCommand $ progDesc "Update flake template url")
      , command "init" (info initCommand $ progDesc "Init flake template")
      , command "new" (info newCommand $ progDesc "New flake template")
      ]

flakeitP :: ParserInfo Command
flakeitP = info (helper <*> commandP) (progDesc "Flake It Program")
