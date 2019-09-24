{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Gauge (bench, bgroup, nf)
import Gauge.Main (defaultMain)

import qualified MTLStyleExample.Main
import           MTLStyleExample.Test.Stubs

import qualified Control.Monad.Freer as Freer
import qualified Control.Monad.Freer.Error as Freer
import qualified Control.Monad.Freer.Input as Freer
import qualified Control.Monad.Freer.Output as Freer
import qualified FreerExample.Test.Stubs as Freer

import qualified Polysemy as Poly
import qualified Polysemy.Error as Poly
import qualified Polysemy.Input as Poly
import qualified Polysemy.Output as Poly
import qualified PolysemyExample.Test.Stubs as Poly

mtl :: ((),[ByteString])
mtl =
  MTLStyleExample.Main.main
  & runArgumentsT ["sample.txt"]
  & runFileSystemT [("sample.txt","Alyssa")]
  & runLoggerT
  & runTickingClockT (posixSecondsToUTCTime 0)
  & runIdentity

freer :: Either String ([ByteString],())
freer =
  MTLStyleExample.Main.main
  & Freer.runArguments
  & Freer.runInputConst ["sample.txt" :: Text]
  & Freer.runFileSystem
  & Freer.runInputConst (Freer.FS [("sample.txt","World")])
  & Freer.runTickingClock (posixSecondsToUTCTime 0) 1
  & Freer.runLogger
  & Freer.runOutputList @ByteString
  & Freer.runError @String
  & Freer.run

polysemy :: Either String ([ByteString],())
polysemy =
  MTLStyleExample.Main.main
  & Poly.runArguments
  & Poly.runInputConst ["sample.txt" :: Text]
  & Poly.runFileSystem
  & Poly.runInputConst (Poly.FS [("sample.txt","World")])
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
main =
  defaultMain [bgroup "effect-style-bench"
                      [bench "mtl" $ nf (runs mtl) 1
                      ,bench "freer" $ nf (runs freer) 1
                      ,bench "polysemy" $ nf (runs polysemy) 1
                      ,bench "mtlN" $ nf (runs mtl) n
                      ,bench "freerN" $ nf (runs freer) n
                      ,bench "polysemyN" $ nf (runs polysemy) n
                      ]]
