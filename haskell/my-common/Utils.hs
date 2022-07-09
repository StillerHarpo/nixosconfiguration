{-# LANGUAGE NoImplicitPrelude #-}
module Utils where

import XMonad.Util.Run (runProcessWithInput)
import Protolude
import qualified Data.Text as T

runProcess :: MonadIO m => Text -> [Text] -> m ()
runProcess x args = void $ runProcessWithInput (T.unpack x) (map T.unpack args) ""
