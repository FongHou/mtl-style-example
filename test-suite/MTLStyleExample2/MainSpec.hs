{-# LANGUAGE DerivingVia #-}

module MTLStyleExample2.MainSpec where

import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import MTLStyleExample.Main

import MTLStyleExample2.Test.Stubs

import Test.Hspec

spec :: Spec
spec = describe "main" $ do
  let epoch = posixSecondsToUTCTime 0
      ((), logMessages) = runTest main
        ["sample.txt"]
        (FileSystem [("sample.txt", "World")])
        epoch
  it "prints two log messages" $ length logMessages `shouldBe` 2
  it "prints a greeting as the first message"
    $ (logMessages !! 0) `shouldBe` "Hello, World!"
  it "prints the elapsed time in milliseconds as the second message"
    $ (logMessages !! 1) `shouldBe` "1000 milliseconds"
