{-# LANGUAGE DeriveAnyClass #-}

module FlakeIt where

import Data.Aeson qualified as JSON
import Data.Aeson.Types (FromJSON, Value)
import Data.Binary
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Process.Typed (proc, readProcessStdout_)

type TemplateName = Text

data TemplateInfo = TemplateInfo
  { description :: Text
  }
  deriving (Generic, FromJSON, Show)

data Flake = Flake
  { templates :: Maybe (Map TemplateName TemplateInfo)
  }
  deriving (Generic, FromJSON, Show)

type TemplateUrl = Text

data Templates = Templates
  { url :: !TemplateUrl
  , names :: ![TemplateName]
  }
  deriving (Generic, Show)

instance Binary Templates

getTemplates :: TemplateUrl -> IO (Maybe Templates)
getTemplates url = do
  out <- readProcessStdout_ $ proc "nix" ["flake", "show", "--json", toString url]
  pure $
    fmap ((\names -> Templates{url, names}) . fmap fst . Map.toList) $
      JSON.decode @Flake out >>= templates

main :: IO ()
main = do
  let url = "github:NixOS/templates"
  let url2 = "github:nix-community/templates"
  Just t1 <- getTemplates url
  Just t2 <- getTemplates url2
  let z = encode [t1, t2]
  writeFileLBS "binary" z

  f <- decodeFile @[Templates] "binary"
  print f

  pass
