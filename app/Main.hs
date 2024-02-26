module Main (main) where

import FlakeIt.Options qualified
import System.IO (hSetEncoding, utf8)

main :: IO ()
main = hSetEncoding stdout utf8 >> FlakeIt.Options.flakeitCli
