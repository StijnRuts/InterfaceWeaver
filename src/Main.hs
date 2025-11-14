module Main (main) where

import qualified Config
import qualified InterfaceWeaver.Internal as Internal
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["ls"] -> Internal.listDevices
    ["list"] -> Internal.listDevices
    ["inspect"] -> putStrLn "Usage: Provide a device path such as /dev/input/eventX"
    ["inspect", devicePath] -> Internal.inspectDevice devicePath
    ["run"] -> Config.main
    [] -> Config.main
    ["-h"] -> putStrLn helpMessage
    ["help"] -> putStrLn helpMessage
    ["--help"] -> putStrLn helpMessage
    options -> do
      putStrLn $ "Unrecognized option " <> show options
      putStrLn helpMessage

helpMessage :: String
helpMessage = "TODO help message"
