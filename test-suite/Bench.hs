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

runFree :: [Text] -> Eff _ a -> AppM a
runFree args = runM . runClock . runFileSystem . runLogger . runArguments
  where
    runArguments = runInputConst args . Freer.runArguments
    runLogger = ignoreOutput @Text . Freer.runLogger
    runFileSystem = subsume @AppM $ \case (ReadFile f) -> MTL.readFile f
    runClock = subsume @AppM $ \case CurrentTime -> currentTime

freer2 :: IO ()
freer2 = MTLStyleExample.Main.main & runFree ["/tmp/world.txt"] & runAppM

mtl :: ((), [ByteString])
mtl = MTLStyleExample.Main.main
  & MTL.runArgumentsT ["sample.txt"]
  & MTL.runFileSystemT [("sample.txt", "Alyssa")]
  & MTL.runLoggerT
  & MTL.runTickingClockT (posixSecondsToUTCTime 0)
  & runIdentity

cps :: ((), [ByteString])
cps = CPS.runTest
  MTLStyleExample.Main.main
  ["sample.txt"]
  (FileSystem [("sample.txt", "World")])
  (posixSecondsToUTCTime 0)

freer :: _
freer = MTLStyleExample.Main.main
  & Freer.runArguments
  & Freer.runInputConst ["sample.txt" :: Text]
  & Freer.runFileSystem
  & Freer.runInputConst (Freer.FS [("sample.txt", "World")])
  & Freer.runTickingClock (posixSecondsToUTCTime 0) 1
  -- & Freer.runError @String
  & Freer.runLogger
  & Freer.runOutputList @Text
  & Freer.runError @String
  & Freer.run

polysemy :: _
polysemy = MTLStyleExample.Main.main
  & Poly.runArguments
  & Poly.runInputConst ["sample.txt" :: Text]
  & Poly.runFileSystem
  & Poly.runInputConst (Poly.FS [("sample.txt", "World")])
  & Poly.runTickingClock (posixSecondsToUTCTime 0) 1
  -- & Poly.runError @String
  & Poly.runLogger
  & Poly.runOutputList @ByteString
  & Poly.runError @String
  & Poly.run

once :: Monad m => m a -> Int -> m _
once m _ = m

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
      [ bench "freer" $ nf (once freer) n
      , bench "polysemy" $ nf (once polysemy) n
      , bench "freer2" $ nfAppIO (once freer2) n
      , bench "mtl" $ nf (once mtl) n
      , bench "cps" $ nf (once cps) n
      , bench "freer/1000" $ nf (runs freer) n
      , bench "polysemy/1000" $ nf (runs polysemy) n
      , bench "freer2/1000" $ nfAppIO (runs freer2) n
      , bench "mtl/1000" $ nf (runs mtl) n
      , bench "cps/1000" $ nf (runs cps) n]]
