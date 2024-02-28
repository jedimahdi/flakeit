module FlakeIt.Nix where

import Data.Aeson qualified as JSON
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import FlakeIt.Types
import System.Process.Typed (proc, readProcessStdout_, runProcess, runProcess_)

getTemplateGroup :: TemplateUrl -> IO (Maybe TemplateGroup)
getTemplateGroup url = do
  out <- readProcessStdout_ $ proc "nix" ["flake", "show", "--json", Text.unpack url]
  pure $
    fmap ((\names -> TemplateGroup{url, names}) . fmap fst . Map.toList) $
      JSON.decode @Flake out >>= templates

initTemplate :: Text -> IO ()
initTemplate p = do
  runProcess_ $ proc "nix" ["flake", "init", "-t", Text.unpack p]

newTemplate :: Text -> Text -> IO ()
newTemplate name p = do
  runProcess_ $ proc "nix" ["flake", "new", Text.unpack name, "-t", Text.unpack p]
