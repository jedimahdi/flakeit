{-# LANGUAGE DeriveAnyClass #-}

module FlakeIt.Types where

import Data.Aeson qualified as JSON
import Data.Aeson.Types (FromJSON, Value)
import Data.Binary

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

data TemplateGroup = TemplateGroup
  { url :: !TemplateUrl
  , names :: ![TemplateName]
  }
  deriving (Generic, Show)

instance Binary TemplateGroup
