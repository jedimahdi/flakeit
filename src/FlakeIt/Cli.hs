module FlakeIt.Cli where

import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Version (Version, showVersion)
import FlakeIt.Cli.Parser
import FlakeIt.DB qualified as DB
import FlakeIt.Nix qualified as Nix
import FlakeIt.Template
import Options.Applicative (execParser)
import Paths_flakeit qualified as Meta (version)
import System.Exit (exitFailure)
import System.IO (stderr)

flakeit :: Version -> (Command -> IO ()) -> IO ()
flakeit version performCommand = execParser (cliParser version) >>= performCommand

flakeitCli :: IO ()
flakeitCli = flakeit Meta.version runCliCommand

runCliCommand :: Command -> IO ()
runCliCommand = \case
  List opts -> runList opts
  Add opts -> runAdd opts
  Remove opts -> runRemove opts
  New opts -> runNew opts
  Init opts -> runInit opts

runList :: ListOptions -> IO ()
runList opts = do
  db <- DB.getAll
  TIO.putStr $ prettyTemplates db

runAdd :: AddOptions -> IO ()
runAdd opts = do
  maybeSource <- Nix.getTemplateSource opts.url
  case maybeSource of
    Just source -> DB.add source
    Nothing -> do
      TIO.hPutStrLn stderr "error: Could not parse the flake"
      exitFailure

runRemove :: RemoveOptions -> IO ()
runRemove opts = DB.remove opts.url

runNew :: NewOptions -> IO ()
runNew opts = Nix.newTemplate opts.projectName opts.template

runInit :: InitOptions -> IO ()
runInit opts = Nix.initTemplate opts.template
