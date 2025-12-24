{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

{- HLint ignore "Redundant <&>" -}
{- HLint ignore "Functor law" -}

{-
import Data.Events
import Data.Functor ((<&>))
import qualified Evdev
import qualified Evdev.Codes as Codes
import InterfaceWeaver.App
import InterfaceWeaver.CLI
import InterfaceWeaver.Evdev
import InterfaceWeaver.Keyboard
import Network.BSD (getHostName)

keyboard :: String -> String
keyboard "P520" = "/dev/input/by-id/usb-Dell_Dell_USB_Entry_Keyboard-event-kbd"
keyboard "T420" = "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
keyboard "X201" = "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
keyboard hostname = error $ "No keyboard defined for " <> hostname

trackpad :: String -> String
trackpad "P520" = "/dev/input/by-id/usb-Apple_Inc._Magic_Trackpad_2_CC2101201T7J2Y1AA-if01-event-mouse"
trackpad "T420" = "/dev/input/by-path/platform-i8042-serio-1-event-mouse"
trackpad "X201" = "/dev/input/by-path/platform-i8042-serio-1-event-mouse"
trackpad hostname = error $ "No trackpad defined for " <> hostname

mouse :: String -> String
mouse "P520" = "/dev/input/by-id/usb-Logitech_USB_Laser_Mouse-event-mouse"
mouse "X201" = "/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-mouse"
mouse hostname = error $ "No mouse defined for " <> hostname

trackpoint :: String -> String
trackpoint "T420" = "/dev/input/by-path/platform-i8042-serio-2-event-mouse"
trackpoint "X201" = "/dev/input/by-path/platform-i8042-serio-2-event-mouse"
trackpoint hostname = error $ "No trackpoint defined for " <> hostname

primaryMouse :: String -> String
primaryMouse "P520" = mouse "P520"
primaryMouse "T420" = trackpad "T420"
primaryMouse "X201" = trackpad "X201"
primaryMouse hostname = error $ "No primary mouse defined for " <> hostname

secondaryMouse :: String -> String
secondaryMouse "P520" = trackpad "P520"
secondaryMouse "T420" = trackpoint "T420"
secondaryMouse "X201" = trackpoint "X201"
secondaryMouse hostname = error $ "No secondary mouse defined for " <> hostname

main :: IO ()
main =
  cli $ do
    hostname <- liftIO getHostName
    let keyboardDevice = keyboard hostname
    let secondaryMouseDevice = secondaryMouse hostname

    deviceSource keyboardDevice True
      <&> mapKeyCodes swapAZ
      >>= withPersistentState "countA" 0 countA
      >>= deviceSink "interfaceweaver"

    deviceSource secondaryMouseDevice False
      >>= liftIO . sink print

swapAZ :: Codes.Key -> Codes.Key
swapAZ Codes.KeyA = Codes.KeyZ
swapAZ Codes.KeyZ = Codes.KeyA
swapAZ kc = kc

countA :: (Evdev.EventData, Int) -> (Evdev.EventData, Int)
countA (event@(Evdev.KeyEvent Codes.KeyA _), state) = (event, state + 1)
countA (event, state) = (event, state)
-}

main :: IO ()
main = putStrLn "Hello"
