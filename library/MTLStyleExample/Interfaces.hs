{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module MTLStyleExample.Interfaces where

import Control.Monad.Freer ( Eff, Member, send )
import Control.Monad.Logger
import Control.Monad.Logger ( MonadLogger(..) )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.State ( StateT )
import Control.Monad.Time ( MonadTime(..) )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad.Writer ( WriterT )

import qualified Data.Text as T
import Data.Text ( Text )
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock ( UTCTime )

import Prelude hiding ( readFile )

import qualified System.Environment as IO
import System.Log.FastLogger ( fromLogStr, toLogStr )

-- | A class of monads that can access command-line arguments.
class Monad m => MonadArguments m where
  -- | Returns the command-line arguments provided to the program.
  getArgs :: m [Text]
  default getArgs :: (MonadTrans t, MonadArguments m', m ~ t m') => m [Text]
  getArgs = lift getArgs

data Arguments a where
   GetArgs :: Arguments [Text]

instance Member Arguments effs => MonadArguments (Eff effs) where
  getArgs = send GetArgs

-- | A class of monads that can interact with the filesystem.
class Monad m => MonadFileSystem m where
  -- | Reads a file at the given path and returns its contents. If the file does
  -- not exist, is not accessible, or is improperly encoded, this method throws
  -- an exception.
  readFile :: Text -> m Text
  default readFile
     :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> m Text
  readFile = lift . readFile

data FileSystem a where
   ReadFile :: Text -> FileSystem Text

instance Member FileSystem effs => MonadFileSystem (Eff effs) where
  readFile = send . ReadFile

data Clock a where
   CurrentTime :: Clock UTCTime

instance (Member Clock effs) => MonadTime (Eff effs) where
  currentTime = send CurrentTime

data Logger a where
   Log :: Text -> Logger ()

instance (Member Logger effs) => MonadLogger (Eff effs) where
  monadLoggerLog _ _ _ str =
     send $ Log $ T.decodeUtf8 (fromLogStr (toLogStr str))

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

