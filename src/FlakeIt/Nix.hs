{-# LANGUAGE DeriveAnyClass #-}

module FlakeIt.Nix where

import Data.Aeson qualified as JSON
import Data.Aeson.Types (FromJSON, Value)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import FlakeIt.Template
import GHC.Generics (Generic)
import System.Process.Typed (proc, readProcessStdout_, runProcess_)

data Flake = Flake
  { templates :: Maybe (Map TemplateName Value)
  }
  deriving (Generic, FromJSON, Show)

getTemplateSource :: SourceUrl -> IO (Maybe Source)
getTemplateSource url = do
  out <- readProcessStdout_ $ proc "nix" ["flake", "show", "--json", urlToString url]
  pure $
    fmap ((\names -> Source{url, names}) . fmap fst . Map.toList) $
      JSON.decode @Flake out >>= templates

initTemplate :: Template -> IO ()
initTemplate template = do
  runProcess_ $ proc "nix" ["flake", "init", "-t", templateToString template]

newTemplate :: String -> Template -> IO ()
newTemplate projectName template = do
  runProcess_ $ proc "nix" ["flake", "new", projectName, "-t", templateToString template]
