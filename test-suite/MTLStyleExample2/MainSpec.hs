{-# LANGUAGE DerivingVia #-}

module MTLStyleExample2.MainSpec where

import Data.Function ((&))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec
import MTLStyleExample.Main
import MTLStyleExample2.Test.Stubs
import MTLStyleExample.Interfaces
import Control.Monad.Logger (MonadLogger(..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Time (MonadTime(..))
import Control.Monad.Writer
import Data.ByteString (ByteString)
import Data.Text (Text)

type Test =
  ReaderT ([Text],FileSystem) (WriterT [ByteString] (State ClockState))

newtype TestM a = TestM {runTest :: Test a}
  deriving (Functor,Applicative,Monad)
  deriving MonadArguments via (ArgumentsT Test)
  deriving MonadLogger via (LoggerT Test)
  deriving MonadFileSystem via (FileSystemT Test)
  deriving MonadTime via (ClockT Test)

spec :: Spec
spec =
  describe "main"
  $ do let epoch = posixSecondsToUTCTime 0
           ((),logMessages) =
             runTest main
             & flip runReaderT
                    (["sample.txt"],FileSystem [("sample.txt","World")])
             & runWriterT
             & runTickingClockT epoch
       -- & runStoppedClockT epoch
       it "prints two log messages" $ length logMessages `shouldBe` 2
       it "prints a greeting as the first message"
         $ (logMessages !! 0) `shouldBe` "Hello, World!"
       it "prints the elapsed time in milliseconds as the second message"
         $ (logMessages !! 1) `shouldBe` "1000 milliseconds"
