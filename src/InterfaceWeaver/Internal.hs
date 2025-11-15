module InterfaceWeaver.Internal where

{- HLint ignore "Redundant <&>" -}

import Control.Category ((>>>))
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Events as Events
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Evdev
import qualified Evdev.Codes as Codes
import InterfaceWeaver.App (interfaceWeaver, run)
import qualified InterfaceWeaver.Evdev as Evdev
import System.Directory (canonicalizePath, doesFileExist, getSymbolicLinkTarget, listDirectory, pathIsSymbolicLink)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

listDevices :: IO ()
listDevices =
  do
    pure
      [ BS.unpack Evdev.evdevDir,
        BS.unpack Evdev.evdevDir </> "by-path",
        BS.unpack Evdev.evdevDir </> "by-id"
      ]
    >>= foldMap listDirectoryFull
    >>= foldMap resolveSymlink
    <&> (Map.fromList >>> Map.elems >>> List.sort)
    >>= foldMap printDeviceInfo
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

inspectDevice :: FilePath -> IO ()
inspectDevice path = interfaceWeaver $ do
  run $ printDeviceInfo path
  run $ Evdev.deviceSource path False >>= Events.sink print

printDeviceInfo :: FilePath -> IO ()
printDeviceInfo path = do
  eitherDevice <- try $ Evdev.newDevice $ BS.pack path
  case eitherDevice of
    Left (_ :: SomeException) -> do
      hPutStrLn stderr $ "Could not read device " <> path <> ", maybe try sudo"
      putStrLn ""
    Right device -> do
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
  where
    showProperty :: Codes.DeviceProperty -> String
    showProperty Codes.InputPropPointer = "Pointer"
    showProperty Codes.InputPropDirect = "Direct"
    showProperty Codes.InputPropButtonpad = "Buttonpad"
    showProperty Codes.InputPropSemiMt = "Semi-multitouch"
    showProperty Codes.InputPropTopbuttonpad = "Topbuttonpad"
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
