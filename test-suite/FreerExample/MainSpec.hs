{-# LANGUAGE TypeApplications #-}

module FreerExample.MainSpec where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.Freer.Trace

import Data.Function ( (&) )
import Data.Text ( Text )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import FreerExample.Test.Stubs

import MTLStyleExample.Interfaces
import MTLStyleExample.Main

import Test.Hspec

spec :: Spec
spec = describe "main" $ do
  let (logs, ()) = either (error "spec error") id
        $ MTLStyleExample.Main.main
        & runArguments
        & runInputConst ["sample.txt" :: Text]
        & runFileSystem
        & runInputConst (FS [("sample.txt", "World")])
        & runTickingClock (posixSecondsToUTCTime 0) 1
        & runLogger
        & runOutputList @Text
        & runError @String
        & run
  it "prints two log messages" $ length logs `shouldBe` 2
  it "prints a greeting as the first message"
    $ (logs !! 0) `shouldBe` "Hello, World!"
  it "prints the elapsed time in milliseconds as the second message"
    $ (logs !! 1) `shouldBe` "1000 milliseconds"

trace :: IO (Either String ([Text], ()))
trace = MTLStyleExample.Main.main
  & traceEffect @Arguments
  & traceEffect @FileSystem
  & traceEffect @Clock
  & traceEffect @Logger
  & runTrace
  & runArguments
  & runInputConst ["sample.txt" :: Text]
  & runFileSystem
  & runInputConst (FS [("sample.txt", "World")])
  & runTickingClock (posixSecondsToUTCTime 0) 1
  & runLogger
  & runOutputList @Text
  & runError @String
  & runM
