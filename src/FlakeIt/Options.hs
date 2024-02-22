module FlakeIt.Options where

import Options.Applicative

data Command
  = List !ListOptions
  | Add !AddOptions
  | Remove !RemoveOptions
  | Update !UpdateOptions
  deriving (Show)

data UpdateOptions = UpdateOptions
  { path :: Text
  }
  deriving (Show)

data RemoveOptions = RemoveOptions
  { path :: Text
  }
  deriving (Show)

data ListOptions = ListOptions
  {}
  deriving (Show)

listOpts :: Parser ListOptions
listOpts = pure ListOptions

listCommand :: Parser Command
listCommand = List <$> listOpts

data AddOptions = AddOptions
  { path :: Text
  }
  deriving (Show)

addOpts :: Parser AddOptions
addOpts = AddOptions <$> argument str (metavar "PATH")

addCommand :: Parser Command
addCommand = Add <$> addOpts

commandP :: Parser Command
commandP =
  subparser $
    command "add" (info (helper <*> addCommand) $ progDesc "Add a flake tempalate url")
      <> command "list" (info (helper <*> listCommand) $ progDesc "List templates")

flakeitP :: ParserInfo Command
flakeitP = info (helper <*> commandP) (progDesc "Flake It Program")
