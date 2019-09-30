{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MonadUnlift where

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

foo :: IO a -> IO a
foo m = do
  x <- m
  pure x

foo' :: MonadUnlift IO m => m a -> m a
foo' m = do
  s <- getInputState
  let m' = runInputState s m
  (r, s') <- liftBase $ foo m'
  putOutputState s'
  pure r

class MonadBase b m => MonadUnlift b m | m -> b where
  type InputState m
  type OutputState m
  getInputState :: m (InputState m)
  runInputState :: InputState m -> m a -> b (a, OutputState m)
  getAndRunInputState :: m a -> m (b (a, OutputState m))
  getAndRunInputState m = do
    s <- getInputState
    return $ runInputState s m
  putOutputState :: OutputState m -> m ()

instance MonadUnlift IO IO where
  type InputState IO = ()
  type OutputState IO = ()
  getInputState = pure ()
  runInputState () m = (, ()) <$> m
  putOutputState () = pure ()

instance MonadUnlift b m => MonadUnlift b (StateT s m) where
  type InputState (StateT s m) = (s, InputState m)
  type OutputState (StateT s m) = (s, OutputState m)

  getInputState = (,) <$> get <*> lift getInputState

  runInputState (s, ss) m = do
    ((r, s'), ss') <- runInputState ss (runStateT m s)
    pure (r, (s', ss'))

  putOutputState (s, ss) = lift (putOutputState ss) *> put s

instance MonadUnlift b m => MonadUnlift b (ReaderT r m) where
  type InputState (ReaderT r m) = (r, InputState m)
  type OutputState (ReaderT r m) = OutputState m
  getInputState = (,) <$> ask <*> lift getInputState 
  runInputState (s, ss) m = runInputState ss (runReaderT m s)
  putOutputState ss = lift (putOutputState ss)

instance (MonadUnlift b m, Monoid w) => MonadUnlift b (WriterT w m) where
  type InputState (WriterT w m) = InputState m
  type OutputState (WriterT w m) = (w, OutputState m)

  getInputState = lift getInputState

  runInputState ss m = do
    ((r, s'), ss') <- runInputState ss (runWriterT m)
    pure (r, (s', ss'))

  putOutputState (s, ss) = lift (putOutputState ss) >> tell s

