{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreerExample.Test.Stubs where

import           Control.Monad.Freer
import           Control.Monad.Freer.Lens
import           Control.Monad.Freer.Input
import           Control.Monad.Freer.Output
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Logger (MonadLogger (..))
import           Control.Monad.Time (MonadTime (..))
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import           GHC.Generics
import           System.Log.FastLogger (fromLogStr, toLogStr)

import MTLStyleExample.Interfaces

--------------------------------------------------------------------------------
-- Arguments

data Arguments a where
  GetArgs :: Arguments [Text]

instance Member Arguments effs => MonadArguments (Eff effs) where
  getArgs = send GetArgs

runArguments :: [Text] -> Eff (Arguments ': eff) x -> Eff eff x
runArguments args = runInputConst args . reinterpret (\case GetArgs -> input)

--------------------------------------------------------------------------------
-- Logger

data Logger a where
  Log :: ByteString -> Logger ()

instance (Member Logger effs) => MonadLogger (Eff effs) where
  monadLoggerLog _ _ _ str = send $ Log (fromLogStr (toLogStr str))

runLogger :: Eff (Logger ': eff) x -> Eff eff ([ByteString], x)
runLogger = runOutputList . reinterpret (\case Log msg -> output msg)

--------------------------------------------------------------------------------
-- FileSystem

data FileSystem a where
  ReadFile :: Text -> FileSystem Text

instance Member FileSystem effs => MonadFileSystem (Eff effs) where
  readFile = send . ReadFile

newtype FS = FS [(Text, Text)]

runFileSystem :: FS -> Eff (FileSystem ': eff) x -> Eff eff (Either Text x)
runFileSystem fs =
  runInputConst fs . runError . reinterpret2
  (\case
     ReadFile path -> do
       FS files <- input @FS
       maybe (throwError $ "readFile: no such file'" <> path <> "'")
             return
             (lookup path files)
  )

--------------------------------------------------------------------------------
-- Clock

data ClockState
  = ClockStopped !UTCTime
  | ClockTick !UTCTime ClockState
  | ClockEndOfTime
  deriving (Eq, Show, Generic)

data Clock a where
  CurrentTime :: Clock UTCTime

instance (Member Clock effs) => MonadTime (Eff effs) where
  currentTime = send CurrentTime

-- runClock
--   :: UTCTime
--   -> ClockState
--   -> Eff (Clock ': eff) x
--   -> Eff eff ClockState
-- runClock t s =
--   evalState s . reinterpret
--   (\case
--     CurrentTime -> do
--       get @ClockState)
--

{-
--------------------------------------------------------------------------------
instance (Member (State ClockState) effs) => MonadTime (Eff effs) where
  currentTime =
    let clock = the @ClockState
    in  ClockT $ use clock >>= \case
          ClockStopped t -> return t
          ClockTick t s  -> clock .= s >> return t
          ClockEndOfTime -> error "currentTime: end of time"

-- -- | Runs a computation with a constant time that never changes.
runStoppedClockT :: UTCTime -> State ClockState a -> a
runStoppedClockT t m = evalState m (ClockStopped t)

-- -- | Runs a computation with a clock that advances by 1 second every time the
-- -- time is read.
runTickingClockT :: UTCTime -> State ClockState a -> a
runTickingClockT = runTickingClockT' 1

-- -- | Runs a computation with a clock that advances by the given interval every
-- -- time the time is read.
runTickingClockT' :: NominalDiffTime -> UTCTime -> State ClockState a -> a
runTickingClockT' d t m = evalState m (ticks t)
  where ticks t' = ClockTick t' (ticks (addUTCTime d t'))

-- -- | Runs a computation with a clock that replays the provided list of times, in
-- -- order. If the list of times is exhausted, 'currentTime' will throw an
-- -- exception the next time it is called.
runPresetClockT :: [UTCTime] -> State ClockState a -> a
runPresetClockT ts m = evalState m (foldr ClockTick ClockEndOfTime ts)
-}
