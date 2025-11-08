module InterfaceWeaver.Keyboard where

import qualified Evdev
import qualified Evdev.Codes as Codes
import InterfaceWeaver.Events (Events)

mapKeyCodes :: (Codes.Key -> Codes.Key) -> Events Evdev.EventData -> Events Evdev.EventData
mapKeyCodes f = fmap f'
  where
    f' (Evdev.KeyEvent code val) = Evdev.KeyEvent (f code) val
    f' eventData = eventData
