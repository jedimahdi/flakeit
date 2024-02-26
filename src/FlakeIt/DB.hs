module FlakeIt.DB (
  add,
  getAll,
  remove,
  clear,
) where

import Data.Binary
import Data.List qualified as List
import FlakeIt.Nix qualified as Nix
import FlakeIt.Types
import System.Directory (
  XdgDirectory (XdgData),
  createDirectoryIfMissing,
  doesFileExist,
  getXdgDirectory,
  removeFile,
 )
import System.FilePath ((</>))
import Prelude hiding (getAll)

clear :: IO ()
clear = do
  dbPath <- getDBPath
  whenM (doesFileExist dbPath) $ removeFile dbPath

add :: TemplateGroup -> IO ()
add tg = do
  dbPath <- getDBPath
  dbExist <- doesFileExist dbPath
  prevDb <- if dbExist then decodeFile @[TemplateGroup] dbPath else pure []
  let newDb = tg : filter (\t -> t.url /= tg.url) prevDb
  encodeFile dbPath newDb

remove :: TemplateUrl -> IO ()
remove url = do
  dbPath <- getDBPath
  dbExist <- doesFileExist dbPath
  prevDb <- if dbExist then decodeFile @[TemplateGroup] dbPath else pure []
  let newDb = filter (\t -> t.url /= url) prevDb
  encodeFile dbPath newDb

getAll :: IO [TemplateGroup]
getAll = do
  dbPath <- getDBPath
  dbExist <- doesFileExist dbPath
  if dbExist then decodeFile @[TemplateGroup] dbPath else pure []

getDBPath :: IO FilePath
getDBPath = do
  dataPath <- getXdgDirectory XdgData "flakeit"
  createDirectoryIfMissing True dataPath
  pure $ dataPath </> "db"
