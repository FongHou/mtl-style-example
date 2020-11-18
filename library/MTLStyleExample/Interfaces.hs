{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MTLStyleExample.Interfaces where

import Control.Monad.Except (MonadTrans (..))
import Control.Monad.Logger (MonadLogger (..))
import Control.Monad.RWS ()
import Control.Monad.Reader ()
import Control.Monad.State ()
import Control.Monad.Writer ()
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Environment as IO
import Prelude hiding (readFile)

-- | A class of monads that can access command-line arguments.
class Monad m => MonadArguments m where
  getArgs :: m [Text]

-- | A class of monads that can interact with the filesystem.
class Monad m => MonadFileSystem m where
  readFile :: Text -> m Text

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadArguments m) => MonadArguments (t m) where
  getArgs = lift getArgs

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadFileSystem m) => MonadFileSystem (t m) where
  readFile = lift . readFile

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadLogger m) => MonadLogger (t m) where
  monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

-------------------------------------------------------------------------------

-- | MTL
instance MonadArguments IO where
  getArgs = map T.pack <$> IO.getArgs

instance MonadFileSystem IO where
  readFile = T.readFile . T.unpack
