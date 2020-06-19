{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Control.Monad.Freer as Freer
import Control.Monad.Freer.Error as Freer
import Control.Monad.Freer.Output as Freer
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import FreerExample.Test.Stubs as Freer
import Gauge
import qualified MTLStyleExample.Main
import MTLStyleExample.Test.Stubs as MTL
import MTLStyleExample2.Test.Stubs as MTL2
import qualified Polysemy as Poly
import qualified Polysemy.Error as Poly
import qualified Polysemy.Input as Poly
import qualified Polysemy.Output as Poly
import qualified PolysemyExample.Test.Stubs as Poly

mtl :: ((), [ByteString])
mtl =
  MTLStyleExample.Main.main
    & MTL.runArgumentsT ["sample.txt"]
    & MTL.runFileSystemT [("sample.txt", "Alyssa")]
    & MTL.runTickingClockT (posixSecondsToUTCTime 0)
    & MTL.runLoggerT
    & runIdentity

mtl2 :: ((), [ByteString])
mtl2 =
  MTL2.runTest
    MTLStyleExample.Main.main
    ["sample.txt"]
    (FileSystem [("sample.txt", "World")])
    (posixSecondsToUTCTime 0)

freer :: (Either String ([Text], ()))
freer =
  MTLStyleExample.Main.main
    & Freer.runArguments ["sample.txt" :: Text]
    & Freer.runFileSystem (Freer.FS [("sample.txt", "World")])
    & Freer.runTickingClock (posixSecondsToUTCTime 0) 1
    & Freer.runLogger
    & Freer.runOutputList @Text
    & Freer.runError @String
    & Freer.run

polysemy :: (Either String ([ByteString], ()))
polysemy =
  MTLStyleExample.Main.main
    & Poly.runArguments
    & Poly.runInputConst ["sample.txt" :: Text]
    & Poly.runFileSystem
    & Poly.runInputConst (Poly.FS [("sample.txt", "World")])
    & Poly.runTickingClock (posixSecondsToUTCTime 0) 1
    & Poly.runLogger
    & Poly.runOutputList @ByteString
    & Poly.runError @String
    & Poly.run

once :: Monad m => m a -> Int -> m a
once m _ = m

runs :: Monad m => m a -> Int -> m Int
runs m n' = do
  replicateM_ n' m
  pure n'

n :: Int
n = 1000

main :: IO ()
main =
  defaultMain
    [ bgroup
        "effect-style-bench"
        [ bench "mtl" $ nf (once mtl) n,
          bench "mtl2" $ nf (once mtl) n,
          bench "freer" $ nf (once freer) n,
          bench "polysemy" $ nf (once polysemy) n,
          bench "mtl/1000" $ nf (runs mtl) n,
          bench "mtl2/1000" $ nf (runs mtl2) n,
          bench "freer/1000" $ nf (runs freer) n,
          bench "polysemy/1000" $ nf (runs polysemy) n
        ]
    ]
