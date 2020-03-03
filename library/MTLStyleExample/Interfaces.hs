{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module MTLStyleExample.Interfaces where

import Control.Monad.Logger
import Control.Monad.Reader ( ReaderT )
import Control.Monad.State ( StateT )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad.Writer ( WriterT )

import qualified Data.Text as T
import Data.Text ( Text )
import qualified Data.Text.IO as T

import Prelude hiding ( readFile )

import qualified System.Environment as IO

-- | A class of monads that can access command-line arguments.
class Monad m => MonadArguments m where
  -- | Returns the command-line arguments provided to the program.
  getArgs :: m [Text]
  default getArgs :: (MonadTrans t, MonadArguments m', m ~ t m') => m [Text]
  getArgs = lift getArgs

-- | A class of monads that can interact with the filesystem.
class Monad m => MonadFileSystem m where
  -- | Reads a file at the given path and returns its contents. If the file does
  -- not exist, is not accessible, or is improperly encoded, this method throws
  -- an exception.
  readFile :: Text -> m Text
  default readFile
    :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> m Text
  readFile = lift . readFile

-------------------------------------------------------------------------------
-- | MTL
instance MonadArguments IO where
  getArgs = map T.pack <$> IO.getArgs

instance MonadFileSystem IO where
  readFile = T.readFile . T.unpack

instance MonadArguments m => MonadArguments (LoggingT m)

instance MonadArguments m => MonadArguments (ReaderT r m)

instance MonadArguments m => MonadArguments (StateT s m)

instance (MonadArguments m, Monoid w) => MonadArguments (WriterT w m)

instance MonadFileSystem m => MonadFileSystem (LoggingT m)

instance MonadFileSystem m => MonadFileSystem (ReaderT r m)

instance MonadFileSystem m => MonadFileSystem (StateT s m)

instance (MonadFileSystem m, Monoid w) => MonadFileSystem (WriterT w m)

