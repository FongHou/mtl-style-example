{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Time ( MonadTime(..) )

import Data.Text ( Text )
import qualified Data.Text as T

import MTLStyleExample.Interfaces as MTL
import qualified MTLStyleExample.Main as MTL

--------------------------------------------------------------------------------
-- IO wiring
newtype AppM a = AppM (LoggingT IO a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadLogger, MonadFileSystem
           , MonadArguments, MonadTime )

runAppM :: AppM a -> IO a
runAppM (AppM x) = runStderrLoggingT x

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

runFree :: Eff _ a -> AppM _
runFree = runM . runClock . runLogger . runFS . runArguments
  where
    runArguments = runInputConst @[Text] ["/tmp/world.txt"]
       . reinterpret (\case GetArgs -> input)
    runLogger = subsume @AppM $ \case (Log s) -> logInfoN s
    -- runFS_ = subsume @AppM $ \case (ReadFile f) -> MTL.readFile f
    runFS = runInputConst (FS [("/tmp/world.txt", "welcome")])
       . runError @String
       . runFileSystem
    runClock = subsume @AppM $ \case CurrentTime -> currentTime

main :: IO ()
main = do
  _ <- runAppM $ runFree MTL.main
  return ()
-- main = runAppM MTLStyleExample.main
