{-# LANGUAGE DataKinds #-}

module Config (main) where

{- HLint ignore "Redundant <&>" -}
{- HLint ignore "Functor law" -}

import Data.Events
import Data.Functor ((<&>))
import qualified Evdev.Codes as Codes
import InterfaceWeaver.Evdev
import InterfaceWeaver.Keyboard

-- p520_keyboard = "dev/input/by-id/usb-Dell_Dell_USB_Entry_Keyboard-event-kbd"
-- p520_trackpad = "/dev/input/by-id/usb-Apple_Inc._Magic_Trackpad_2_CC2101201T7J2Y1AA-event-mouse"
-- p520_mouse = "/dev/input/by-id/usb-Logitech_USB_Laser_Mouse-event-mouse"
-- t420_keyboard = "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
-- t420_trackpad = "/dev/input/by-path/platform-i8042-serio-1-event-mouse"
-- t420_trackpoint = "/dev/input/by-path/platform-i8042-serio-2-event-mouse"
-- x201_keyboard = "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
-- x201_trackpad = "/dev/input/by-path/platform-i8042-serio-1-event-mouse"
-- x201_trackpoint = "/dev/input/by-path/platform-i8042-serio-2-event-mouse"
-- x201_mouse = "/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-mouse"

t420_keyboard :: String
t420_keyboard = "/dev/input/by-path/platform-i8042-serio-0-event-kbd"

t420_trackpoint :: String
t420_trackpoint = "/dev/input/by-path/platform-i8042-serio-2-event-mouse"

main :: IO ()
main = do
  deviceSource t420_keyboard True
    <&> relax
    <&> mapKeyCodes swapAZ
    <&> specialize
    >>= deviceSink "interfaceweaver"

  deviceSource t420_trackpoint False
    >>= sink print

  keepAlive

swapAZ :: Codes.Key -> Codes.Key
swapAZ Codes.KeyA = Codes.KeyZ
swapAZ Codes.KeyZ = Codes.KeyA
swapAZ kc = kc
