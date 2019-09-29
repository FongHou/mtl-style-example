import Control.Monad.Freer
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Time ( MonadTime(..) )

import Data.Text ( Text )

import MTLStyleExample.Interfaces as MTL
import qualified MTLStyleExample.Main as MTL

--------------------------------------------------------------------------------
-- IO wiring
newtype AppM a = AppM (LoggingT IO a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadLogger, MonadFileSystem
           , MonadArguments, MonadTime )

runAppM :: AppM a -> IO a
runAppM (AppM x) = runStderrLoggingT x

runFree
   :: [Text] -> Eff '[Arguments, Logger, FileSystem, Clock, AppM] a -> AppM a
runFree args = runM . runClock . runFileSystem . runLogger . runArguments
  where
    runArguments = runInputConst @[Text] args
       . reinterpret (\case GetArgs -> input)
    runLogger = ignoreOutput @Text
       . reinterpret (\case Log s -> output s)
    runFileSystem = subsume @AppM $ \case (ReadFile f) -> MTL.readFile f
    runClock = subsume @AppM $ \case CurrentTime -> currentTime

main :: IO ()
main = runAppM $ runFree ["/tmp/world.txt"] MTL.main
-- main = runAppM MTLStyleExample.main
