{-# LANGUAGE DataKinds #-}

module Main (main) where

{- HLint ignore "Redundant <&>" -}
{- HLint ignore "Functor law" -}

import Data.Events
import Data.Functor ((<&>))
import qualified Evdev
import qualified Evdev.Codes as Codes
import InterfaceWeaver.App
import InterfaceWeaver.Evdev
import InterfaceWeaver.Internal
import InterfaceWeaver.Keyboard

-- p520_keyboard = "/dev/input/by-id/usb-Dell_Dell_USB_Entry_Keyboard-event-kbd"
-- p520_trackpad = "/dev/input/by-id/usb-Apple_Inc._Magic_Trackpad_2_CC2101201T7J2Y1AA-if01-event-mouse"
-- p520_mouse = "/dev/input/by-id/usb-Logitech_USB_Laser_Mouse-event-mouse"
-- t420_keyboard = "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
-- t420_trackpad = "/dev/input/by-path/platform-i8042-serio-1-event-mouse"
-- t420_trackpoint = "/dev/input/by-path/platform-i8042-serio-2-event-mouse"
-- x201_keyboard = "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
-- x201_trackpad = "/dev/input/by-path/platform-i8042-serio-1-event-mouse"
-- x201_trackpoint = "/dev/input/by-path/platform-i8042-serio-2-event-mouse"
-- x201_mouse = "/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-mouse"

p520_keyboard :: String
p520_keyboard = "/dev/input/by-id/usb-Dell_Dell_USB_Entry_Keyboard-event-kbd"

p520_trackpad :: String
p520_trackpad = "/dev/input/by-id/usb-Apple_Inc._Magic_Trackpad_2_CC2101201T7J2Y1AA-if01-event-mouse"

main :: IO ()
main =
  interfaceWeaver $ do
    countState <- withPersistentState "countA" 0 countA

    run $
      deviceSource p520_keyboard True
        <&> mapKeyCodes swapAZ
        <&> countState
        >>= deviceSink "interfaceweaver"

    run $
      deviceSource p520_trackpad False
        >>= sink print

swapAZ :: Codes.Key -> Codes.Key
swapAZ Codes.KeyA = Codes.KeyZ
swapAZ Codes.KeyZ = Codes.KeyA
swapAZ kc = kc

countA :: (Evdev.EventData, Int) -> (Evdev.EventData, Int)
countA (event@(Evdev.KeyEvent Codes.KeyA _), state) = (event, state + 1)
countA (event, state) = (event, state)
