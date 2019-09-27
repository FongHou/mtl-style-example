{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module FreerExample.Test.Stubs where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.Freer.State
import Control.Monad.Logger ( MonadLogger(..) )
import Control.Monad.Time ( MonadTime(..) )

import Data.ByteString ( ByteString )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Time.Clock ( NominalDiffTime, UTCTime, addUTCTime )

import GHC.Generics

import MTLStyleExample.Interfaces

import System.Log.FastLogger ( fromLogStr, toLogStr )

runArguments
   :: Member (Input [Text]) eff => Eff (Arguments : eff) x -> Eff eff x
runArguments = interpret $ \case GetArgs -> input

--------------------------------------------------------------------------------
-- Logger
data Logger a where
   Log :: ByteString -> Logger ()

instance (Member Logger effs) => MonadLogger (Eff effs) where
  monadLoggerLog _ _ _ str = send $ Log (fromLogStr (toLogStr str))

runLogger
   :: Member (Output ByteString) eff => Eff (Logger : eff) x -> Eff eff x
runLogger = interpret $ \case Log msg -> output msg

--------------------------------------------------------------------------------
-- FileSystem
newtype FS = FS [(Text, Text)]

runFileSystem :: Members [Input FS, (Error String)] eff
              => Eff (FileSystem : eff) x
              -> Eff eff x
runFileSystem = interpret $ \case
  ReadFile path -> do
    FS files <- input @FS
    maybe
      (throwError $ "readFile: no such file '" <> T.unpack path <> "'")
      return
      (lookup path files)

--------------------------------------------------------------------------------
-- Clock
data ClockState
   = ClockStopped !UTCTime
   | ClockTick !UTCTime ClockState
   | ClockEndOfTime
  deriving ( Eq, Show, Generic )

data Clock a where
   CurrentTime :: Clock UTCTime

instance (Member Clock effs) => MonadTime (Eff effs) where
  currentTime = send CurrentTime

runClock :: Member (Error String) eff
         => Eff (Clock : eff) x
         -> Eff (State ClockState : eff) x
runClock = reinterpret $ \case
  CurrentTime -> get >>= \case
    ClockStopped t' -> return t'
    ClockTick t' s  -> put s >> return t'
    ClockEndOfTime  -> throwError @String "currentTime: end of time"

runTickingClock :: Member (Error String) eff
                => UTCTime
                -> NominalDiffTime
                -> Eff (Clock : eff) x
                -> Eff eff x
runTickingClock t d = evalState (ticks t) . runClock
  where
    ticks t' = ClockTick t' (ticks (addUTCTime d t'))

runStoppedClock
   :: Member (Error String) eff => UTCTime -> Eff (Clock : eff) x -> Eff eff x
runStoppedClock t = evalState (ClockStopped t) . runClock

runPresetClock :: Member (Error String) eff
               => [UTCTime]
               -> Eff (Clock : eff) x
               -> Eff eff x
runPresetClock ts = evalState (foldr ClockTick ClockEndOfTime ts) . runClock
