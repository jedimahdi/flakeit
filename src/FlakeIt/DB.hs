module FlakeIt.DB (
  add,
  getAll,
  remove,
  clear,
) where

import Control.Monad (when)
import Data.Binary
import Data.List qualified as List
import FlakeIt.Nix qualified as Nix
import FlakeIt.Template
import System.Directory (
  XdgDirectory (XdgData),
  createDirectoryIfMissing,
  doesFileExist,
  getXdgDirectory,
  removeFile,
 )
import System.FilePath ((</>))

clear :: IO ()
clear = do
  dbPath <- defaultDBPath
  doesExist <- doesFileExist dbPath
  when doesExist $ removeFile dbPath

add :: Source -> IO ()
add source = do
  dbDirectory <- defaultDBDirectory
  createDirectoryIfMissing True dbDirectory
  dbPath <- defaultDBPath
  prevDb <- readDB dbPath
  let newDb = source : filter (\s -> s.url /= source.url) prevDb
  writeDB dbPath newDb

remove :: SourceUrl -> IO ()
remove url = do
  dbPath <- defaultDBPath
  prevDb <- readDB dbPath
  let newDb = filter (\t -> t.url /= url) prevDb
  writeDB dbPath newDb

getAll :: IO [Source]
getAll = defaultDBPath >>= readDB

readDB :: FilePath -> IO [Source]
readDB dbPath = do
  dbExist <- doesFileExist dbPath
  if dbExist then decodeFile @[Source] dbPath else pure []

writeDB :: FilePath -> [Source] -> IO ()
writeDB = encodeFile

defaultDBDirectory :: IO FilePath
defaultDBDirectory = getXdgDirectory XdgData "flakeit"

defaultDBPath :: IO FilePath
defaultDBPath = fmap (</> "db") defaultDBDirectory
