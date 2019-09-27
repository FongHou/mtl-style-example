{-# LANGUAGE TypeApplications #-}

module FreerExample.MainSpec where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output

import Data.ByteString ( ByteString )
import Data.Function ( (&) )
import Data.Text ( Text )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import FreerExample.Test.Stubs

import MTLStyleExample.Main

import Test.Hspec

spec :: Spec
spec = describe "main" $ do
  let Right (logs, ()) = MTLStyleExample.Main.main
        & runArguments
        & runFileSystem
        & runLogger
        & runTickingClock (posixSecondsToUTCTime 0) 1
        & runOutputList @ByteString
        & runError @String
        & runInputConst (FS [("sample.txt", "World")])
        & runInputConst ["sample.txt" :: Text]
        & run
  it "prints two log messages" $ length logs `shouldBe` 2
  it "prints a greeting as the first message"
    $ (logs !! 0) `shouldBe` "Hello, World!"
  it "prints the elapsed time in milliseconds as the second message"
    $ (logs !! 1) `shouldBe` "1000 milliseconds"
