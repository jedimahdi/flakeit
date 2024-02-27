module FlakeIt.Cli where

import Data.List qualified as List
import Data.Text qualified as Text
import Data.Version (Version, showVersion)
import FlakeIt.Cli.Parser
import FlakeIt.DB qualified as DB
import FlakeIt.Nix qualified as Nix
import FlakeIt.Template
import FlakeIt.Types
import Options.Applicative
import Paths_flakeit qualified as Meta (version)

flakeit :: Version -> (Command -> IO ()) -> IO ()
flakeit version performCommand = execParser (cliParser version) >>= performCommand

flakeitCli :: IO ()
flakeitCli = flakeit Meta.version runCliCommand

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
