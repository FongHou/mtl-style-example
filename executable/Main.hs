import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Time (MonadTime (..))
import MTLStyleExample.Interfaces as MTL
import qualified MTLStyleExample.Main as MTL

--------------------------------------------------------------------------------
-- IO wiring
newtype AppT m a = AppT (LoggingT m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadLogger,
      MonadFileSystem,
      MonadArguments,
      MonadTime
    )

runAppM :: MonadIO m => AppT m a -> m a
runAppM (AppT x) = runStderrLoggingT x

main :: IO ()
main = runAppM MTL.main
