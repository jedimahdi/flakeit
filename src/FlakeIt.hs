{-# LANGUAGE PartialTypeSignatures #-}

module FlakeIt where

import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BS
import Data.Text qualified as Text
import FlakeIt.DB qualified as DB
import FlakeIt.Nix qualified as Nix
import FlakeIt.Options
import FlakeIt.Types
import Options.Applicative (execParser)

listTemplates :: TemplateGroup -> [Text]
listTemplates t = map (\name -> t.url <> "#" <> name) t.names

prettyTemplates :: [TemplateGroup] -> Text
prettyTemplates = unlines . concatMap listTemplates

runCommand :: Command -> IO ()
runCommand (Add opts) = do
  -- TODO: Find duplicates
  maybeTemplates <- Nix.getTemplateGroup opts.path
  case maybeTemplates of
    Just ts -> DB.add ts
    Nothing -> do
      error "Could not find templates..."
runCommand (List opts) = do
  db <- DB.getAll
  putText $ prettyTemplates db
runCommand (Remove opts) = do
  DB.remove opts.path
runCommand (Update opts) = do
  maybeTemplates <- Nix.getTemplateGroup opts.path
  case maybeTemplates of
    Just ts -> DB.replace opts.path ts
    Nothing -> do
      error "Could not find templates..."

main :: IO ()
main = do
  command <- execParser flakeitP
  runCommand command
