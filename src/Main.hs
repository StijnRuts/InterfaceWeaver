module Main (main) where

import qualified Config
import qualified InterfaceWeaver.Evdev as Evdev
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["ls"] -> Evdev.listDevices
    ["list"] -> Evdev.listDevices
    ["inspect"] -> putStrLn "Usage: Provide a device path such as /dev/input/eventX"
    ["inspect", devicePath] -> Evdev.inspectDevice devicePath
    ["run"] -> Config.main
    [] -> Config.main
    ["-h"] -> putStrLn helpMessage
    ["help"] -> putStrLn helpMessage
    ["--help"] -> putStrLn helpMessage
    options -> do
      putStrLn $ "Unrecognized option " <> show options
      putStrLn helpMessage

helpMessage :: String
helpMessage = "help message"
