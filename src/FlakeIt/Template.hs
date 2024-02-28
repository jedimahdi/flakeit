{-# LANGUAGE DerivingStrategies #-}

module FlakeIt.Template where

import Data.Aeson (FromJSON, FromJSONKey)
import Data.Binary (Binary)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

data Source = Source
  { url :: !SourceUrl
  , names :: ![TemplateName]
  }
  deriving (Generic, Show)

instance Binary Source

makeTemplate :: SourceUrl -> TemplateName -> Template
makeTemplate url name = Template $ urlToText url <> "#" <> nameToText name

listTemplates :: Source -> [Template]
listTemplates source = map (makeTemplate source.url) source.names

prettyTemplates :: [Source] -> Text
prettyTemplates = Text.unlines . map templateToText . concatMap listTemplates

newtype SourceUrl = SourceUrl Text
  deriving newtype (Show, Eq, Binary, IsString)

urlToText :: SourceUrl -> Text
urlToText (SourceUrl url) = url

urlToString :: SourceUrl -> String
urlToString (SourceUrl url) = Text.unpack url

newtype TemplateName = TemplateName Text
  deriving newtype (Show, Eq, Ord, Binary, FromJSONKey)

nameToText :: TemplateName -> Text
nameToText (TemplateName name) = name

nameToString :: TemplateName -> String
nameToString (TemplateName name) = Text.unpack name

newtype Template = Template Text
  deriving newtype (Show, IsString)

templateToText :: Template -> Text
templateToText (Template template) = template

templateToString :: Template -> String
templateToString = Text.unpack . templateToText
