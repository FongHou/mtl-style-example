{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import Control.Monad
import Control.Monad.Freer as Freer
import Control.Monad.Freer.Error as Freer
import Control.Monad.Freer.Input as Freer
import Control.Monad.Freer.Output as Freer
import Control.Monad.Freer.Trace as Freer
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Time ( MonadTime(..) )

import Data.ByteString ( ByteString )
import Data.Function ( (&) )
import Data.Functor.Identity ( runIdentity )
import Data.Text ( Text )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import qualified FreerExample.Test.Stubs as Freer

import Gauge
import Gauge.Main ( defaultMain )

import MTLStyleExample.Interfaces as MTL
import qualified MTLStyleExample.Main
import MTLStyleExample.Test.Stubs as MTL

import MTLStyleExample2.Test.Stubs as CPS

import qualified Polysemy as Poly
import qualified Polysemy.Error as Poly
import qualified Polysemy.Input as Poly
import qualified Polysemy.Output as Poly

import qualified PolysemyExample.Test.Stubs as Poly

--------------------------------------------------------------------------------
-- IO wiring
newtype AppM a = AppM (LoggingT IO a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadLogger, MonadFileSystem
           , MonadArguments, MonadTime )

runAppM :: AppM a -> IO a
runAppM (AppM x) = runStderrLoggingT x

runFree :: [Text] -> Eff _ a -> AppM _
runFree args = runM . runClock . runFileSystem . runLogger . runArguments
  where
    runArguments = runInputConst args . Freer.runArguments
    runLogger = ignoreOutput @Text . Freer.runLogger
    runFileSystem = subsume @AppM $ \case (ReadFile f) -> MTL.readFile f
    runClock = subsume @AppM $ \case CurrentTime -> currentTime


freer2 :: IO _
freer2 = MTLStyleExample.Main.main & runFree ["/tmp/world.txt"] & runAppM

mtl :: ((), [ByteString])
mtl = MTLStyleExample.Main.main
   & MTL.runArgumentsT ["sample.txt"]
   & MTL.runFileSystemT [("sample.txt", "Alyssa")]
   & MTL.runLoggerT
   & MTL.runTickingClockT (posixSecondsToUTCTime 0)
   & runIdentity

cps :: ((), [ByteString])
cps = MTLStyleExample.Main.main
   & CPS.runTest
   & CPS.runArgumentsFileSystem
     ["sample.txt"]
     (FileSystem [("sample.txt", "World")])
   & CPS.runLoggerT
   & CPS.runTickingClockT (posixSecondsToUTCTime 0)

freer :: Either String _
freer = MTLStyleExample.Main.main
   & Freer.runArguments
   & Freer.runInputConst ["sample.txt" :: Text]
   & Freer.runFileSystem
   & Freer.runInputConst (Freer.FS [("sample.txt", "World")])
   & Freer.runTickingClock (posixSecondsToUTCTime 0) 1
   & Freer.runError @String
   & Freer.runLogger
   & Freer.outputToTrace @Text
   & Freer.ignoreTrace
   & Freer.run

polysemy :: Either String ([ByteString], ())
polysemy = MTLStyleExample.Main.main
   & Poly.runArguments
   & Poly.runInputConst ["sample.txt" :: Text]
   & Poly.runFileSystem
   & Poly.runInputConst (Poly.FS [("sample.txt", "World")])
   & Poly.runTickingClock (posixSecondsToUTCTime 0) 1
   & Poly.runLogger
   & Poly.runOutputList @ByteString
   & Poly.runError @String
   & Poly.run

runs :: Monad m => m a -> Int -> m Int
runs m n' = do
  replicateM_ n' m
  pure n'

n :: Int
n = 1000

main :: IO ()
main = defaultMain
  [ bgroup
      "effect-style-bench"
      [ bench "freer" $ whnf (runs freer) 1
      , bench "freer2" $ whnf (runs freer) 1
      , bench "mtl" $ whnf (runs mtl) 1
      , bench "cps" $ whnf (runs cps) 1
      , bench "polysemy" $ whnf (runs polysemy) 1
      , bench "freerN" $ nf (runs freer) n
      , bench "freer2N" $ nfAppIO (runs freer2) n
      , bench "mtlN" $ nf (runs mtl) n
      , bench "cpsN" $ nf (runs cps) n
      , bench "polysemyN" $ nf (runs polysemy) n]]
