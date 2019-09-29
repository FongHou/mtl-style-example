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
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Time.Clock ( NominalDiffTime, UTCTime, addUTCTime )

import GHC.Generics

import MTLStyleExample.Interfaces


runArguments
   :: Member (Input [Text]) eff => Eff (Arguments : eff) x -> Eff eff x
runArguments = interpret $ \case GetArgs -> input

--------------------------------------------------------------------------------
-- Logger
runLogger
   :: Member (Output Text) eff => Eff (Logger : eff) x -> Eff eff x
runLogger = interpret $ \case Log msg -> output msg

-- runLogger :: Eff (Logger : r) x -> Eff r ([Text], x)
-- runLogger m = stateful (\case Log msg -> S.modify' (msg:)) [] m

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

runClock :: Member (Error String) eff
         => Eff (Clock : eff) x
         -> Eff (State ClockState : eff) x
runClock = reinterpret $ \case
  CurrentTime -> get >>= \case
    ClockStopped t' -> return t'
    ClockTick t' s  -> put s >> return t'
    ClockEndOfTime  -> throwError @String "currentTime: end of time"
{-# INLINE runClock #-}

runTickingClock :: Member (Error String) eff
                => UTCTime
                -> NominalDiffTime
                -> Eff (Clock : eff) x
                -> Eff eff x
runTickingClock t d = evalState (ticks t) . runClock
  where
    ticks t' = ClockTick t' (ticks (addUTCTime d t'))
{-# INLINE runTickingClock #-}

runStoppedClock
   :: Member (Error String) eff => UTCTime -> Eff (Clock : eff) x -> Eff eff x
runStoppedClock t = evalState (ClockStopped t) . runClock

runPresetClock :: Member (Error String) eff
               => [UTCTime]
               -> Eff (Clock : eff) x
               -> Eff eff x
runPresetClock ts = evalState (foldr ClockTick ClockEndOfTime ts) . runClock
