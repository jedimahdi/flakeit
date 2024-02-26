{-# LANGUAGE LambdaCase #-}

module FlakeIt.Options where

import Data.List qualified as List
import Data.Text qualified as Text
import Data.Version (Version, showVersion)
import FlakeIt.DB qualified as DB
import FlakeIt.Nix qualified as Nix
import FlakeIt.Types
import Options.Applicative
import Paths_flakeit qualified as Meta (version)

flakeit :: Version -> (Command -> IO ()) -> IO ()
flakeit version performCommand = execParser (cliParser version) >>= performCommand

flakeitCli :: IO ()
flakeitCli = flakeit Meta.version runCliCommand

listTemplates :: TemplateGroup -> [Text]
listTemplates t = map (\name -> t.url <> "#" <> name) t.names

prettyTemplates :: [TemplateGroup] -> Text
prettyTemplates = unlines . concatMap listTemplates

addTemplateGroup :: TemplateUrl -> IO ()
addTemplateGroup url = do
  maybeTemplates <- Nix.getTemplateGroup url
  case maybeTemplates of
    Just ts -> DB.add ts
    Nothing -> error "Could not find templates..."

runList :: ListOptions -> IO ()
runList opts = do
  db <- DB.getAll
  putText $ prettyTemplates db

runAdd :: AddOptions -> IO ()
runAdd opts = addTemplateGroup opts.path

runUpdate :: UpdateOptions -> IO ()
runUpdate opts = addTemplateGroup opts.path

runRemove :: RemoveOptions -> IO ()
runRemove opts = DB.remove opts.path

runNew :: NewOptions -> IO ()
runNew opts = Nix.newTemplate opts.name opts.template

runInit :: InitOptions -> IO ()
runInit opts = Nix.initTemplate opts.template

runCliCommand :: Command -> IO ()
runCliCommand = \case
  List opts -> runList opts
  Add opts -> runAdd opts
  Update opts -> runUpdate opts
  Remove opts -> runRemove opts
  New opts -> runNew opts
  Init opts -> runInit opts

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
    map toString . concatMap listTemplates <$> DB.getAll

pathCompleter :: Completer
pathCompleter = mkCompleter comp
 where
  comp :: String -> IO [String]
  comp s =
    filter (\t -> s `isPrefixOf` t) . map (toString . (\l -> l.url)) <$> DB.getAll

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
newCommand = New <$> newOpts

data InitOptions = InitOptions
  { template :: !Text
  }
  deriving (Show)

initOpts :: Parser InitOptions
initOpts =
  InitOptions
    <$> strOption (long "template" <> short 't' <> help "Choose template to use" <> completer templateCompleter)

initCommand :: Parser Command
initCommand = Init <$> initOpts

data UpdateOptions = UpdateOptions
  { path :: Text
  }
  deriving (Show)

updateOpts :: Parser UpdateOptions
updateOpts =
  UpdateOptions
    <$> argument str (completer pathCompleter <> metavar "PATH")

updateCommand :: Parser Command
updateCommand = Update <$> updateOpts

data RemoveOptions = RemoveOptions
  { path :: Text
  }
  deriving (Show)

removeOpts :: Parser RemoveOptions
removeOpts = RemoveOptions <$> argument str (metavar "PATH")

removeCommand :: Parser Command
removeCommand = Remove <$> removeOpts

data ListOptions = ListOptions
  {}
  deriving (Show)

listOpts :: Parser ListOptions
listOpts = pure ListOptions

listCommand :: Parser Command
listCommand = List <$> listOpts

data AddOptions = AddOptions
  { path :: Text
  }
  deriving (Show)

addOpts :: Parser AddOptions
addOpts = AddOptions <$> argument str (metavar "PATH")

addCommand :: Parser Command
addCommand = Add <$> addOpts

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
