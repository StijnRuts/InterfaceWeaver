module InterfaceWeaver.Keyboard where

import Data.Events (Events)
import Data.Union (Member, ReplaceMember, Union (..), inject, project, replace)
import qualified Evdev
import qualified Evdev.Codes as Codes

mapKeyCodes :: (ReplaceMember Evdev.EventData Evdev.EventData r r) => (Codes.Key -> Codes.Key) -> Events (Union r) -> Events (Union r)
mapKeyCodes f = fmap $ replace f'
  where
    f' :: Evdev.EventData -> Evdev.EventData
    f' (Evdev.KeyEvent code val) = Evdev.KeyEvent (f code) val
    f' eventData = eventData
