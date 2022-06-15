{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Xrandr where
import Protolude
import qualified Data.Text as T
import Utils

data MonitorStatus = MonitorStatus
  { isConnected :: Bool,
    isEnabled :: Bool
  }
  deriving (Eq)

monitorStatus :: MonadIO m => Text -> m MonitorStatus
monitorStatus display = liftIO $ do
  isConnected <-
    (/= "disconnected") . T.strip <$> readFile (pathPrefix <> T.unpack display <> "/status")
  isEnabled <-
    (/= "disabled") . T.strip <$> readFile (pathPrefix <> T.unpack display <> "/enabled")
  pure MonitorStatus {..}
  where
    pathPrefix = "/sys/class/drm/card0-"

updateMonitor :: MonadIO m => m ()
updateMonitor = do
  toDisable <-
    filterM
      (fmap (\x -> isEnabled x && not (isConnected x)) . monitorStatus)
      ["DP-1", "DP-2", "HDMI-A-1", "HDMI-A-2"]
  let disableOptions =
        concatMap (\x -> ["--output", sysToRandr x, "--off"]) toDisable
  toEnable <-
    findM
      (fmap (\x -> not (isEnabled x) && isConnected x) . monitorStatus)
      ["DP-1", "DP-2", "HDMI-A-1", "HDMI-A-2"]
  let enableOption = maybe eDP1EnableOption toEnableOption toEnable
  runProcess "xrandr" (disableOptions <> enableOption)

eDP1EnableOption :: [Text]
eDP1EnableOption = ["--output", "eDP-1", "--auto"]

sysToRandr :: Text -> Text
sysToRandr "HDMI-A-1" = "HDMI-1"
sysToRandr "HDMI-A-2" = "HDMI-2"
sysToRandr x = x

toEnableOption :: Text -> [Text]
toEnableOption extMonitor =
  eDP1EnableOption
    <> [ "--below",
         sysToRandr extMonitor,
         "--output",
         sysToRandr extMonitor,
         "--primary"
       ]

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)
