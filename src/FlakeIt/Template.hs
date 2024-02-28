module FlakeIt.Template where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (Version, showVersion)
import FlakeIt.DB qualified as DB
import FlakeIt.Nix qualified as Nix
import FlakeIt.Types
import Options.Applicative
import Paths_flakeit qualified as Meta (version)

listTemplates :: TemplateGroup -> [Text]
listTemplates t = map (\name -> t.url <> "#" <> name) t.names

prettyTemplates :: [TemplateGroup] -> Text
prettyTemplates = Text.unlines . concatMap listTemplates
