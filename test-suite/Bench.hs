{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Data.Functor.Identity (runIdentity)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Gauge (bench,bgroup,nf)
import Gauge.Main (defaultMain)
import qualified MTLStyleExample.Main
import FreerExample.Test.Stubs
import MTLStyleExample.Test.Stubs

freer :: Either String ([ByteString],())
freer =
  MTLStyleExample.Main.main
  & runArguments
  & runInputConst ["sample.txt" :: Text]
  & runFileSystem
  & runInputConst (FS [("sample.txt","World")])
  & runTickingClock (posixSecondsToUTCTime 0) 1
  & runLogger
  & runOutputList @ByteString
  & runError @String
  & run

mtl :: ((),[ByteString])
mtl =
  MTLStyleExample.Main.main
  & runArgumentsT ["sample.txt"]
  & runFileSystemT [("sample.txt","Alyssa")]
  & runLoggerT
  & runTickingClockT (posixSecondsToUTCTime 0)
  & runIdentity

runs :: Int
runs = 1000

main :: IO ()
main =
  defaultMain [bgroup "effect-style-bench"
                      [bench "mtl" $ nf (const mtl) runs
                      ,bench "freer" $ nf (const freer) runs
                      ,bench "mtlN" $ nf (\n -> replicateM_ n mtl) runs
                      ,bench "freerN" $ nf (\n -> replicateM_ n freer) runs]]
