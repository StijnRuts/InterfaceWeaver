module InterfaceWeaver.CLI (cli) where

{- HLint ignore "Redundant <&>" -}
{- HLint ignore "Monad law, left identity" -}
{- HLint ignore "Monad law, right identity" -}

{-
import Control.Category ((>>>))
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import Data.Events (Events)
import qualified Data.Events as Events
import Data.Functor (($>), (<&>))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Evdev
import qualified Evdev.Codes as Codes
import InterfaceWeaver.App (App, liftIO, runApp)
import qualified InterfaceWeaver.Evdev as Evdev
import System.Directory (canonicalizePath, doesFileExist, getSymbolicLinkTarget, listDirectory, pathIsSymbolicLink)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

cli :: App () -> IO ()
cli app = do
  args <- getArgs
  case args of
    ["ls"] -> listDevices
    ["list"] -> listDevices
    ["detect"] -> runApp detectDevices
    ["inspect"] -> putStrLn "Usage: Provide a device path such as /dev/input/eventX"
    ["inspect", devicePath] -> runApp $ inspectDevice devicePath
    ["run"] -> runApp app
    [] -> runApp app
    ["-h"] -> putStrLn helpMessage
    ["help"] -> putStrLn helpMessage
    ["--help"] -> putStrLn helpMessage
    options -> do
      putStrLn $ "Unrecognized option " <> show options
      putStrLn helpMessage

helpMessage :: String
helpMessage =
  unlines
    [ "list: print a list of all devices found",
      "detect: print the path of a device when an events is detected",
      "inspect PATH: show all events for the device at PATH",
      "run: run your app configuration",
      "help: display this help message"
    ]

type DeviceInfo = (FilePath, Evdev.Device)

getDevice :: FilePath -> IO (Maybe DeviceInfo)
getDevice path = do
  eitherDevice <- try $ Evdev.newDevice $ BS.pack path
  case eitherDevice of
    Right device -> return $ Just (path, device)
    Left (_ :: SomeException) -> return Nothing

getDevices :: IO [DeviceInfo]
getDevices =
  pure
    [ BS.unpack Evdev.evdevDir,
      BS.unpack Evdev.evdevDir </> "by-path",
      BS.unpack Evdev.evdevDir </> "by-id"
    ]
    >>= foldMap listDirectoryFull
    >>= foldMap resolveSymlink
    <&> deduplicateSymlinks
    >>= mapM getDevice
    <&> catMaybes
  where
    listDirectoryFull :: FilePath -> IO [FilePath]
    listDirectoryFull dir = do
      files <- listDirectory dir
      pure [dir </> file | file <- files]

    resolveSymlink :: FilePath -> IO [(FilePath, FilePath)]
    resolveSymlink path = do
      isFile <- doesFileExist path
      isLink <- pathIsSymbolicLink path
      case () of
        _
          | not isFile -> pure []
          | not isLink -> pure [(path, path)]
          | otherwise -> do
              target <- getSymbolicLinkTarget path
              targetPath <- canonicalizePath (takeDirectory path </> target)
              pure [(targetPath, path)]

    deduplicateSymlinks :: [(FilePath, FilePath)] -> [FilePath]
    deduplicateSymlinks = Map.fromList >>> Map.elems >>> List.sort

listDevices :: IO ()
listDevices = do
  devices <- getDevices
  if null devices
    then hPutStrLn stderr "Could not read any devices"
    else foldMap printDeviceInfo devices

detectDevices :: App ()
detectDevices = do
  devices <- liftIO getDevices
  if null devices
    then liftIO $ hPutStrLn stderr "Could not read any devices"
    else
      return devices
        >>= foldMap toPathEvents
        >>= suppressRepeats
        >>= liftIO . Events.sink putStr
  where
    toPathEvents :: DeviceInfo -> App (Events String)
    toPathEvents (path, _) = Evdev.deviceSource path False <&> ($> path)
    suppressRepeats :: Events String -> App (Events String)
    suppressRepeats = Events.withState "" (\(path, active) -> (if path /= active then "\n" <> path <> "\n" else ".", path))

inspectDevice :: FilePath -> App ()
inspectDevice path = do
  maybeDeviceInfo <- liftIO $ getDevice path
  case maybeDeviceInfo of
    Nothing -> liftIO $ hPutStrLn stderr $ "Could not read device " <> path
    Just deviceInfo -> do
      liftIO $ printDeviceInfo deviceInfo
      Evdev.deviceSource path False >>= liftIO . Events.sink print

printDeviceInfo :: DeviceInfo -> IO ()
printDeviceInfo (path, device) = do
  putStrLn $ "Device: " <> path
  name <- Evdev.deviceName device
  putStrLn $ "  Name: " <> BS.unpack name
  properties <- Evdev.deviceProperties device
  unless (null properties) $ do
    putStrLn "  Properties:"
    mapM_ (putStrLn . (\p -> "    - " ++ showProperty p)) properties
  eventTypes <- Evdev.deviceEventTypes device
  let filteredEventTypes = List.filter (/= Codes.EvSyn) eventTypes
  unless (null filteredEventTypes) $ do
    putStrLn "  Events:"
    mapM_ (putStrLn . (\p -> "    - " ++ showEventType p)) filteredEventTypes
  putStrLn ""

showProperty :: Codes.DeviceProperty -> String
showProperty Codes.InputPropPointer = "Pointer"
showProperty Codes.InputPropDirect = "Direct"
showProperty Codes.InputPropButtonpad = "Buttonpad"
showProperty Codes.InputPropTopbuttonpad = "Topbuttonpad"
showProperty Codes.InputPropSemiMt = "Semi-multitouch"
showProperty Codes.InputPropPointingStick = "Pointing stick"
showProperty Codes.InputPropAccelerometer = "Accelerometer"

showEventType :: Codes.EventType -> String
showEventType Codes.EvSyn = "Sync"
showEventType Codes.EvKey = "Key"
showEventType Codes.EvRel = "Relative Movement"
showEventType Codes.EvAbs = "Absolute Movement"
showEventType Codes.EvMsc = "Misc"
showEventType Codes.EvSw = "Switch"
showEventType Codes.EvLed = "LED"
showEventType Codes.EvSnd = "Sound"
showEventType Codes.EvRep = "Repeat"
showEventType Codes.EvFf = "Force Feedback"
showEventType Codes.EvFfStatus = "Force Feedback Status"
showEventType Codes.EvPwr = "Power"
-}
