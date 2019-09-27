{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MTLStyleExample2.Test.Stubs where

import Control.Monad.Logger ( MonadLogger(..) )
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Time ( MonadTime(..) )
import Control.Monad.Writer.CPS

import Data.ByteString ( ByteString )
import Data.Generics.Product
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Time.Clock ( NominalDiffTime, UTCTime, addUTCTime )

import GHC.Generics

import Lens.Micro.Platform

import MTLStyleExample.Interfaces

import System.Log.FastLogger ( fromLogStr, toLogStr )

type Test m = ReaderT ([Text], FS) (WriterT [ByteString] (StateT ClockState m))

newtype TestT m a = TestM { runTest :: Test m a }
  deriving ( Functor, Applicative, Monad )
  deriving MonadArguments via (ArgumentsT (Test m))
  deriving MonadLogger via (LoggerT (Test m))
  deriving MonadFileSystem via (FileSystemT (Test m))
  deriving MonadTime via (ClockT (Test m))

--------------------------------------------------------------------------------
-- Arguments
newtype ArgumentsT m a = ArgumentsT (m a)
  deriving ( Functor, Applicative, Monad )

instance (MonadReader r m, HasType [Text] r)
   => MonadArguments (ArgumentsT m) where
  getArgs = ArgumentsT $ view (typed @[Text])

--------------------------------------------------------------------------------
-- File System
newtype FileSystemT m a = FileSystemT (m a)
  deriving ( Functor, Applicative, Monad )

newtype FS = FileSystem [(Text, Text)]

instance (MonadReader r m, HasType FS r)
   => MonadFileSystem (FileSystemT m) where
  readFile
    path = FileSystemT $ view (typed @FS) >>= \(FileSystem files) -> maybe
    (error $ "readFile: no such file ‘" ++ T.unpack path ++ "’")
    return
    (lookup path files)

runArgumentsFileSystem :: [Text] -> FS -> ReaderT ([Text], FS) m a -> m a
runArgumentsFileSystem args fs = flip runReaderT (args, fs)

--------------------------------------------------------------------------------
-- Logger
newtype LoggerT m a = LoggerT (m a)
  deriving ( Functor, Applicative, Monad )

instance (MonadWriter [ByteString] m) => MonadLogger (LoggerT m) where
  monadLoggerLog _ _ _ str = LoggerT $ tell [fromLogStr (toLogStr str)]

runLoggerT :: WriterT [ByteString] m a -> m (a, [ByteString])
runLoggerT = runWriterT

--------------------------------------------------------------------------------
-- Clock
data ClockState
   = ClockStopped !UTCTime
   | ClockTick !UTCTime ClockState
   | ClockEndOfTime
  deriving ( Eq, Show, Generic )

newtype ClockT m a = ClockT (m a)
  deriving ( Functor, Applicative, Monad )

instance (MonadState s m, HasType ClockState s) => MonadTime (ClockT m) where
  currentTime = let clock = the @ClockState
                in ClockT $ use clock >>= \case
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
  where
    ticks t' = ClockTick t' (ticks (addUTCTime d t'))

-- -- | Runs a computation with a clock that replays the provided list of times, in
-- -- order. If the list of times is exhausted, 'currentTime' will throw an
-- -- exception the next time it is called.
runPresetClockT :: [UTCTime] -> State ClockState a -> a
runPresetClockT ts m = evalState m (foldr ClockTick ClockEndOfTime ts)
