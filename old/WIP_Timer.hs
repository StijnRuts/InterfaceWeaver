{-
data WithTimeout e t = Event e | Timeout t

runWithTimeout :: ConduitT (WithTimeout e t) o (ReaderT TimerManager m) () -> ConduitT e o m ()
runWithTimeout inConduit =
  bracketP
  -- IO (TMChan t)
  newTMChanIO
  -- ((TMChan t) -> IO ())
  atomically . closeTMChan
  -- ((TMChan t) -> ConduitT e o m ())
  \chan ->
    tm <- getSystemTimerManager
    let inConduit' = runReaderC tm inConduit -- ConduitT (WithTimeout e t) o m ()
    let timeoutConduit = sourceTMChan chan .| C.map Timeout -- ConduitT () (WithTimeout e t) m ()
    ??? -- ConduitT e o m ())

tk <- registerTimeout 1_000_000 $ atomically . writeTMChan chan value

-}
