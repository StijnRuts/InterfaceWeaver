module Weaver.Timer where

import Data.Conduit

data WithTimer e t = Event e | Timeout t

data TimerT t m = TimerT -- TODO

runTimerT :: (MonadIO m) => TimerT t m -> ConduitT i o (TimerT t m) () -> ConduitT i o m ()
runTimerT timerT conduit = _

-- tm <- getSystemTimerManager
-- tk <- registerTimeout 1_000_000 $ atomically . writeTMChan chan value

runWithTimeout :: (MonadResource m) => ConduitT (WithTimer e t) o (TimerT t m) () -> ConduitT e o m ()
runWithTimeout myConduit = bracketP
  newTMChanIO -- IO (TMChan t)
  (atomically . closeTMChan) -- ((TMChan t) -> IO ())
  $ \chan ->
    -- ((TMChan t) -> ConduitT e o m ())
    let myConduit' = runTimerT chan myConduit -- ConduitT (WithTimer e t) o m ()
        eventsConduit = mapC Event -- ConduitT e (WithTimer e t) m ()
        timeoutConduit = sourceTMChan chan .| C.map Timeout -- ConduitT () (WithTimer e t) m ()
        timeoutConduit' = mapInput (const ()) (const Nothing) timeoutConduit -- ConduitT e (WithTimer e t) m ()
        inConduit = getZipConduit $ ZipConduit eventsConduit <> ZipConduit timeoutConduit' -- ConduitT e (WithTimer e t) m ()
     in inConduit .| myConduit' -- ConduitT e o m ())
