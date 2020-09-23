{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MTLStyleExample.Interfaces where

import Control.Monad.Logger
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Environment as IO
import Prelude hiding (readFile)

-- | A class of monads that can access command-line arguments.
class Monad m => MonadArguments m where
  getArgs :: m [Text]
  default getArgs ::
    (MonadTrans t, MonadArguments m', m ~ t m') =>
    m [Text]
  getArgs = lift getArgs

-- | A class of monads that can interact with the filesystem.
class Monad m => MonadFileSystem m where
  readFile :: Text -> m Text
  default readFile ::
    (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    Text ->
    m Text
  readFile = lift . readFile

instance
  {-# OVERLAPPABLE #-}
  (Monad (t m), MonadTrans t, MonadArguments m) =>
  MonadArguments (t m)
  where
  getArgs = lift getArgs

instance
  {-# OVERLAPPABLE #-}
  (Monad (t m), MonadTrans t, MonadFileSystem m) =>
  MonadFileSystem (t m)
  where
  readFile = lift . readFile

instance
  {-# OVERLAPPABLE #-}
  (Monad (t m), MonadTrans t, MonadLogger m) =>
  MonadLogger (t m)
  where
  monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

-------------------------------------------------------------------------------
-- | MTL
instance MonadArguments IO where
  getArgs = map T.pack <$> IO.getArgs

instance MonadFileSystem IO where
  readFile = T.readFile . T.unpack

-- instance MonadArguments m => MonadArguments (LoggingT m)
-- instance MonadArguments m => MonadArguments (ExceptT e m)
-- instance MonadArguments m => MonadArguments (ReaderT r m)
-- instance MonadArguments m => MonadArguments (StateT s m)
-- instance (MonadArguments m, Monoid w) => MonadArguments (WriterT w m)
-- instance (MonadArguments m, Monoid w) => MonadArguments (RWST r w s m)
-- instance MonadFileSystem m => MonadFileSystem (LoggingT m)
-- instance MonadFileSystem m => MonadFileSystem (ReaderT r m)
-- instance MonadFileSystem m => MonadFileSystem (StateT s m)
-- instance (MonadFileSystem m, Monoid w) => MonadFileSystem (WriterT w m)
-- instance (MonadFileSystem m, Monoid w) => MonadFileSystem (RWST r w s m)