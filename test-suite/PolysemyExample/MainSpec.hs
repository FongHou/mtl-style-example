{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PolysemyExample.MainSpec where

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec
import PolysemyExample.Test.Stubs
import MTLStyleExample.Main

spec :: Spec
spec =
  describe "main"
  $ do let Right (logs,()) =
             MTLStyleExample.Main.main
             & runArguments
             & runLogger
             & runTickingClock (posixSecondsToUTCTime 0) 1
             & runFileSystem
             & runOutputList @ByteString
             & runError @String
             & runInputConst (FS [("sample.txt","World")])
             & runInputConst ["sample.txt" :: Text]
             & run
       it "prints two log messages" $ length logs `shouldBe` 2
       it "prints a greeting as the first message"
         $ (logs !! 0) `shouldBe` "Hello, World!"
       it "prints the elapsed time in milliseconds as the second message"
         $ (logs !! 1) `shouldBe` "1000 milliseconds"
