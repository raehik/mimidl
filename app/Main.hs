module Main where

import Mimidl.Cli.Parse qualified as Cli
import Mimidl.Cli.Run qualified as Cli

main :: IO ()
main = Cli.parse >>= Cli.run
