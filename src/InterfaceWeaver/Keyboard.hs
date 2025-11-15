module InterfaceWeaver.Keyboard where

import Data.Events (Events)
import qualified Evdev
import qualified Evdev.Codes as Codes

mapKeyCodes :: (Codes.Key -> Codes.Key) -> Events Evdev.EventData -> Events Evdev.EventData
mapKeyCodes f = fmap f'
  where
    f' :: Evdev.EventData -> Evdev.EventData
    f' (Evdev.KeyEvent code val) = Evdev.KeyEvent (f code) val
    f' eventData = eventData
