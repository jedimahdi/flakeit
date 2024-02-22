module FlakeIt.Nix where

import Data.Aeson qualified as JSON
import Data.Map.Strict qualified as Map
import FlakeIt.Types
import System.Process.Typed (proc, readProcessStdout_)

getTemplateGroup :: TemplateUrl -> IO (Maybe TemplateGroup)
getTemplateGroup url = do
  out <- readProcessStdout_ $ proc "nix" ["flake", "show", "--json", toString url]
  pure $
    fmap ((\names -> TemplateGroup{url, names}) . fmap fst . Map.toList) $
      JSON.decode @Flake out >>= templates
