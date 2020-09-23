{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MTLStyleExample.Test.Stubs where

import Control.Monad.Logger (MonadLogger (..), logInfoN)
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Time (MonadTime (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Writer (WriterT (..), tell)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import MTLStyleExample.Interfaces
import System.Log.FastLogger (fromLogStr, toLogStr)

--------------------------------------------------------------------------------
-- Arguments
newtype ArgumentsT m a = ArgumentsT (ReaderT [Text] m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Runs a computation with access to a set of command-line arguments.
runArgumentsT :: [Text] -> ArgumentsT m a -> m a
runArgumentsT args (ArgumentsT x) = runReaderT x args

instance Monad m => MonadArguments (ArgumentsT m) where
  getArgs = ArgumentsT ask

--------------------------------------------------------------------------------
-- File System
newtype FileSystemT m a = FileSystemT (ReaderT [(Text, Text)] m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Runs a computation that may interact with the file system, given a mapping
-- from file paths to file contents.
runFileSystemT :: [(Text, Text)] -> FileSystemT m a -> m a
runFileSystemT fs (FileSystemT x) = runReaderT x fs

instance Monad m => MonadFileSystem (FileSystemT m) where
  readFile path =
    FileSystemT $
      ask >>= \files -> do
        maybe
          (error $ "readFile: no such file ‘" ++ T.unpack path ++ "’")
          return
          (lookup path files)

--------------------------------------------------------------------------------
-- Logger
newtype LoggerT m a = LoggerT (WriterT [ByteString] m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Runs a computation that may emit log messages, returning the result of the
-- computation combined with the set of messages logged, in order.
runLoggerT :: LoggerT m a -> m (a, [ByteString])
runLoggerT (LoggerT x) = runWriterT x

instance Monad m => MonadLogger (LoggerT m) where
  monadLoggerLog _ _ _ str = LoggerT $ tell [fromLogStr (toLogStr str)]

--------------------------------------------------------------------------------
-- Clock
data ClockState
  = ClockStopped !UTCTime
  | ClockTick !UTCTime ClockState
  | ClockEndOfTime
  deriving (Eq, Show)

newtype ClockT m a = ClockT (StateT ClockState m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Runs a computation with a constant time that never changes.
runStoppedClockT :: Monad m => UTCTime -> ClockT m a -> m a
runStoppedClockT t (ClockT x) = evalStateT x (ClockStopped t)

-- | Runs a computation with a clock that advances by 1 second every time the
-- time is read.
runTickingClockT :: Monad m => UTCTime -> ClockT m a -> m a
runTickingClockT = runTickingClockT' 1

-- | Runs a computation with a clock that advances by the given interval every
-- time the time is read.
runTickingClockT' :: Monad m => NominalDiffTime -> UTCTime -> ClockT m a -> m a
runTickingClockT' d t (ClockT x) = evalStateT x (ticks t)
  where
    ticks t' = ClockTick t' (ticks (addUTCTime d t'))

-- | Runs a computation with a clock that replays the provided list of times, in
-- order. If the list of times is exhausted, 'currentTime' will throw an
-- exception the next time it is called.
runPresetClockT :: Monad m => [UTCTime] -> ClockT m a -> m a
runPresetClockT ts (ClockT x) =
  evalStateT x (foldr ClockTick ClockEndOfTime ts)

instance (MonadLogger m) => MonadTime (ClockT m) where
  currentTime =
    ClockT $
      get >>= \case
        ClockStopped t -> logInfoN (T.pack $ show t) >> return t
        ClockTick t s -> put s >> return t
        ClockEndOfTime -> error "currentTime: end of time"
