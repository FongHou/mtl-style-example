{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module PolysemyExample.Test.Stubs where

import Control.Monad.Logger ( MonadLogger(..) )
import Control.Monad.Time ( MonadTime(..) )

import Data.ByteString ( ByteString )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Time.Clock ( NominalDiffTime, UTCTime, addUTCTime )

import GHC.Generics

import MTLStyleExample.Interfaces

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.State

import System.Log.FastLogger ( fromLogStr, toLogStr )

--------------------------------------------------------------------------------
-- Arguments
data Arguments m a where
   GetArgs_ :: Arguments m [Text]

makeSem ''Arguments

instance Member Arguments effs => MonadArguments (Sem effs) where
  getArgs = getArgs_

runArguments
   :: Member (Input [Text]) eff => Sem (Arguments : eff) x -> Sem eff x
runArguments = interpret (\case GetArgs_ -> input)

--------------------------------------------------------------------------------
-- Logger
data Logger m a where
   Log_ :: ByteString -> Logger m ()

makeSem ''Logger

instance Member Logger effs => MonadLogger (Sem effs) where
  monadLoggerLog _ _ _ str = log_ (fromLogStr (toLogStr str))

runLogger
   :: Member (Output ByteString) eff => Sem (Logger : eff) x -> Sem eff x
runLogger = interpret (\case Log_ msg -> output msg)

--------------------------------------------------------------------------------
-- FileSystem
data FileSystem m a where
   ReadFile_ :: Text -> FileSystem m Text

makeSem ''FileSystem

instance Member FileSystem effs => MonadFileSystem (Sem effs) where
  readFile = readFile_

newtype FS = FS [(Text, Text)]

runFileSystem :: Members [Input FS, (Error String)] eff
              => Sem (FileSystem : eff) x
              -> Sem eff x
runFileSystem = interpret
  (\case ReadFile_ path -> do
           FS files <- input @FS
           maybe
             (throw $ "readFile: no such file '" <> T.unpack path <> "'")
             return
             (lookup path files))

--------------------------------------------------------------------------------
-- Clock
data ClockState
   = ClockStopped !UTCTime
   | ClockTick !UTCTime ClockState
   | ClockEndOfTime
  deriving ( Eq, Show, Generic )

data Clock m a where
   CurrentTime_ :: Clock m UTCTime

makeSem ''Clock

instance Member Clock effs => MonadTime (Sem effs) where
  currentTime = currentTime_

runClock :: Member (Error String) eff
         => Sem (Clock : eff) x
         -> Sem (State ClockState : eff) x
runClock = reinterpret
  (\case CurrentTime_ -> get >>= \case
           ClockStopped t' -> return t'
           ClockTick t' s  -> put s >> return t'
           ClockEndOfTime  -> throw @String "currentTime: end of time")

runTickingClock :: Member (Error String) eff
                => UTCTime
                -> NominalDiffTime
                -> Sem (Clock : eff) x
                -> Sem eff x
runTickingClock t d = evalState (ticks t) . runClock
  where
    ticks t' = ClockTick t' (ticks (addUTCTime d t'))

runStoppedClock
   :: Member (Error String) eff => UTCTime -> Sem (Clock : eff) x -> Sem eff x
runStoppedClock t = evalState (ClockStopped t) . runClock

runPresetClock :: Member (Error String) eff
               => [UTCTime]
               -> Sem (Clock : eff) x
               -> Sem eff x
runPresetClock ts = evalState (foldr ClockTick ClockEndOfTime ts) . runClock
