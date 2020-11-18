{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module MTLStyleExample2.Test.Stubs where

import Control.Monad.Identity
import Control.Monad.Logger
import Control.Monad.RWS.CPS
import Control.Monad.Time (MonadTime (..))
import Data.ByteString (ByteString)
import Data.Generics.Product
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime)
import GHC.Generics
import Lens.Micro.Platform
import MTLStyleExample.Interfaces

type RWST' = RWST ([Text], FS) [ByteString] (ClockState, [ByteString])

newtype TestT m a = Test (RWST' m a)
  deriving (Functor, Applicative, Monad, MonadTrans)
  deriving (MonadArguments) via ArgumentsT (RWST' m)
  deriving (MonadFileSystem) via FileSystemT (RWST' m)
  deriving (MonadLogger) via LoggerT (RWST' m)
  deriving (MonadTime) via ClockT (RWST' m)

runTest :: Monad m => TestT m a -> [Text] -> FS -> UTCTime -> m (a, [ByteString])
runTest (Test m) args fs t = evalRWST m (args, fs) (ticks t, [])
  where
    ticks t' = ClockTick t' (ticks (addUTCTime 1 t'))

--------------------------------------------------------------------------------
-- Arguments
newtype ArgumentsT m a = ArgumentsT (m a)
  deriving (Functor, Applicative, Monad)
  deriving (MonadTrans) via IdentityT

instance (MonadReader r m, HasType [Text] r) => MonadArguments (ArgumentsT m) where
  getArgs = ArgumentsT $ view (the @[Text])

--------------------------------------------------------------------------------
-- File System
newtype FileSystemT m a = FileSystemT (m a)
  deriving (Functor, Applicative, Monad)
  deriving (MonadTrans) via IdentityT

newtype FS = FileSystem [(Text, Text)]

instance (MonadReader r m, HasType FS r) => MonadFileSystem (FileSystemT m) where
  readFile path =
    FileSystemT $
      view (the @FS) >>= \(FileSystem files) -> do
        maybe
          (error $ "readFile: no such file ‘" ++ T.unpack path ++ "’")
          return
          (lookup path files)

--------------------------------------------------------------------------------
-- Logger
newtype LoggerT m a = LoggerT (m a)
  deriving (Functor, Applicative, Monad)
  deriving (MonadTrans) via IdentityT

instance (MonadWriter [ByteString] m) => MonadLogger (LoggerT m) where
  monadLoggerLog _ _ _ str = LoggerT $ tell [fromLogStr (toLogStr str)]

--------------------------------------------------------------------------------
-- Clock
data ClockState
  = ClockStopped !UTCTime
  | ClockTick !UTCTime ClockState
  | ClockEndOfTime
  deriving (Eq, Show, Generic)

newtype ClockT m a = ClockT (m a)
  deriving (Functor, Applicative, Monad)
  deriving (MonadTrans) via IdentityT

instance (MonadState s m, HasType ClockState s) => MonadTime (ClockT m) where
  currentTime =
    let clock = the @ClockState
     in ClockT $
          use clock >>= \case
            ClockStopped t -> return t
            ClockTick t s -> clock .= s >> return t
            ClockEndOfTime -> error "currentTime: end of time"
