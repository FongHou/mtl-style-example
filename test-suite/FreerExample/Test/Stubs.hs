{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreerExample.Test.Stubs
  ( runArguments,
    runLogger,
    runFileSystem,
    runTickingClock,
    runStoppedClock,
    runPresetClock,
    Arguments,
    Clock,
    Logger,
    FileSystem,
    FS (..),
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.Freer.State
import Control.Monad.Logger
import Control.Monad.Time (MonadTime (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import GHC.Generics
import MTLStyleExample.Interfaces

data Arguments a where
  GetArgs :: Arguments [Text]

deriving instance Show (Arguments a)

instance Member Arguments effs => MonadArguments (Eff effs) where
  getArgs = send GetArgs

runArguments :: [Text] -> Eff (Arguments : eff) x -> Eff eff x
runArguments x =
  runInputConst x
    . reinterpret
      (\case GetArgs -> input)

--------------------------------------------------------------------------------
-- Logger
data Logger a where
  Log :: Text -> Logger ()

deriving instance Show (Logger a)

instance (Member Logger effs) => MonadLogger (Eff effs) where
  monadLoggerLog _ _ _ str =
    send $ Log $ T.decodeUtf8 (fromLogStr (toLogStr str))

runLogger :: Member (Output Text) eff => Eff (Logger : eff) x -> Eff eff x
runLogger = interpret $ \case
  Log msg -> output msg

--------------------------------------------------------------------------------
-- FileSystem
data FileSystem a where
  ReadFile :: Text -> FileSystem Text

deriving instance Show (FileSystem a)

instance Member FileSystem effs => MonadFileSystem (Eff effs) where
  readFile = send . ReadFile

newtype FS = FS [(Text, Text)]

runFileSystem ::
  Members '[Error String] eff =>
  FS ->
  Eff (FileSystem : eff) x ->
  Eff eff x
runFileSystem fs =
  runInputConst fs
    . reinterpret
      ( \case
          ReadFile path -> do
            FS files <- input @FS
            maybe
              (throwError $ "readFile: no such file '" <> T.unpack path <> "'")
              return
              (lookup path files)
      )

--------------------------------------------------------------------------------
-- Clock

data Clock a where
  CurrentTime :: Clock UTCTime

deriving instance Show (Clock a)

instance (Member Clock effs) => MonadTime (Eff effs) where
  currentTime = send CurrentTime

data ClockState
  = ClockStopped !UTCTime
  | ClockTick !UTCTime ClockState
  | ClockEndOfTime
  deriving (Eq, Show, Generic)

runClock ::
  Member (Error String) eff =>
  Eff (Clock : eff) x ->
  Eff (State ClockState : eff) x
runClock = reinterpret $ \case
  CurrentTime ->
    get >>= \case
      ClockStopped t' -> return t'
      ClockTick t' s -> put s >> return t'
      ClockEndOfTime -> throwError @String "currentTime: end of time"

runTickingClock ::
  Member (Error String) eff =>
  UTCTime ->
  NominalDiffTime ->
  Eff (Clock : eff) x ->
  Eff eff x
runTickingClock t d = evalState (ticks t) . runClock
  where
    ticks t' = ClockTick t' (ticks (addUTCTime d t'))

runStoppedClock ::
  Member (Error String) eff => UTCTime -> Eff (Clock : eff) x -> Eff eff x
runStoppedClock t = evalState (ClockStopped t) . runClock

runPresetClock ::
  Member (Error String) eff => [UTCTime] -> Eff (Clock : eff) x -> Eff eff x
runPresetClock ts = evalState (foldr ClockTick ClockEndOfTime ts) . runClock
