{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module PolysemyExample.MainSpec where

import Data.ByteString ( ByteString )
import Data.Function ( (&) )
import Data.Text ( Text )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import MTLStyleExample.Main

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output

import PolysemyExample.Test.Stubs

import Test.Hspec

spec :: Spec
spec = describe "main" $ do
  let (logs, ()) = either (error "spec error") id
        $ MTLStyleExample.Main.main
        & runArguments
        & runLogger
        & runTickingClock (posixSecondsToUTCTime 0) 1
        & runFileSystem
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
