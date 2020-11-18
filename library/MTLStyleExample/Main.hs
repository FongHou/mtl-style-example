module MTLStyleExample.Main (main) where

import Control.Monad.Logger (MonadLogger (..), logInfoN)
import Control.Monad.Time (MonadTime (..))
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import MTLStyleExample.Interfaces
import Prelude hiding (readFile)

main ::
  ( MonadArguments m,
    MonadFileSystem m,
    MonadLogger m,
    MonadTime m
  ) =>
  m ()
main = do
  startTime <- currentTime
  fileName <- getArgs
  target <- readFile $ head fileName
  logInfoN $ "Hello, " <> target <> "!"
  endTime <- currentTime
  let duration = endTime `diffUTCTime` startTime
  logInfoN $
    T.pack (show (round (duration * 1000) :: Integer)) <> " milliseconds"
