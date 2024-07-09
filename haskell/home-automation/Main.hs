{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import System.IO
import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char
import Data.Maybe (fromMaybe)
import Network.MQTT.Client
import Network.MQTT.Topic
import Network.URI (URI)
import Network.URI.Static (staticURI)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Aeson
import HomeAutomation
import Control.Exception (bracket)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

-- send multiple signals to bulb according to step duration. This is
-- necessary because some bulbs have an upper transition time
publishLightSteps :: TQueue Logger -> MQTTClient -> TimeOfDay -> Light -> IO ()
publishLightSteps queue client tod light = do
  let (startTemp, steps) = timeToColorTemp tod light
  publishLight queue client (topic light <> setT) startTemp
  forM_ steps $ \changeLight -> do
    publishLight queue client (topic light <> setT) changeLight
    threadDelay (maybe 0 (* (1000 * 1000)) $ transition changeLight)

-- publish a mqtt message to a light
publishLight :: TQueue Logger -> MQTTClient -> Topic -> ChangeLight -> IO ()
publishLight queue client topic light = do
  atomically
    . writeTQueue queue
    . LogMessage
    $ "Changing light for " <> unTopic topic <> ": " <> T.decodeUtf8 (BS.toStrict (encode light))
  publish client topic (encode light) False

getCurrentTimeOfDay :: IO TimeOfDay
getCurrentTimeOfDay = do
  time <- getCurrentTime
  timeZone <- getTimeZone time
  pure $ localTimeOfDay $ utcToLocalTime timeZone time

colorTempRunner :: TQueue Logger -> MQTTClient -> [Light] -> IO ()
colorTempRunner queue client lights = do
  forever $ do
    timeBeforeSleep <- getCurrentTimeOfDay
    let seconds = minimum $ map (timeUntil timeBeforeSleep) [dayTimeStart, nightTimeStart]
    threadDelay (fromIntegral $ diffTimeToMicroseconds seconds)
    lightChangeTime <- getCurrentTimeOfDay
    forM_ lights $ publishLightSteps queue client lightChangeTime

mosquittoURI :: URI
mosquittoURI = $$(staticURI "mqtt://localhost")

runMqtt :: TQueue Logger -> IO ()
runMqtt queue = 
  bracket
    (connectURI mqttConfig {_msgCB = SimpleCallback (handleMessage queue)} mosquittoURI)
    (\mc -> do
      stopClient mc
      atomically $ writeTQueue queue StopLogging)
    (\mc -> do
        subscribe mc (map ((, subOptions) . toFilter) [
                   "zigbee2mqtt/switch/kitchen/action",
                   "zigbee2mqtt/switch/livingRoom/action",
                   "zigbee2mqtt/switch/hallway/action",
                   "zigbee2mqtt/switch/bedroom/action",
                   "zigbee2mqtt/bridge/event",
                   kitchenOneT, kitchenTwoT, kitchenThreeT, livingRoomT, bedroomT, hallwayT, dinerRoomT]) []
        colorTempRunner queue mc [livingRoom, kitchen, hallway, dinerRoom, bedroom]
        waitForClient mc)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering 
  queue <- atomically newTQueue
  withAsync (runMqtt queue) $ \_ -> do
    logger queue
    putStrLn "Terminating program"

data Logger =
  LogMessage T.Text
  | StopLogging

logger :: TQueue Logger -> IO ()
logger queue = atomically (readTQueue queue) >>= \case
  LogMessage msg -> do
    T.putStrLn msg
    logger queue
  StopLogging -> pure ()


handleMessage :: TQueue Logger -> MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
handleMessage queue client reasonTopic m _ = do
  atomically . writeTQueue queue . LogMessage $ "Handle message of topic: " <> unTopic reasonTopic
  time <- getCurrentTimeOfDay
  when (reasonTopic == "zigbee2mqtt/switch/kitchen/action") $ handleSwitch queue kitchenAndDinerRoom client m
  when (reasonTopic == "zigbee2mqtt/switch/livingRoom/action") $ handleSwitch queue livingRoom client m
  when (reasonTopic == "zigbee2mqtt/switch/hallway/action") $ handleSwitch queue hallway client m
  when (reasonTopic == "zigbee2mqtt/switch/bedroom/action") $ handleSwitch queue bedroom client m
  when (reasonTopic == "zigbee2mqtt/bridge/event") $
    case decode m of
      Nothing ->
        atomically
        . writeTQueue queue
        . LogMessage
        $ "Parsing of bridge event failed: " <> T.decodeUtf8 (BS.toStrict m)
      Just BridgeEvent{eventKind = BridgeEventDeviceAnnounce, friendlyName = Just (mkTopic -> Just name)} -> do
        atomically . writeTQueue queue . LogMessage $ "got Event with name: " <> unTopic name
        forM_ (matchLight name) $ \light -> do
          atomically . writeTQueue queue $ LogMessage "matched light"
          publishLight queue client (name <> setT) (changeLight { state = Just ON })
          publishLightSteps queue client time light
      Just ev -> atomically . writeTQueue queue . LogMessage $ "Ignoring event: " <> T.pack (show ev)

-- | Handles which light one switch handles
handleSwitch :: TQueue Logger -> Light -> MQTTClient -> ByteString -> IO ()
handleSwitch queue light client m = do
  time <- getCurrentTimeOfDay
  when (m == "on_press_release" || m == "on") $ do
    publishLight queue client (topic light <> setT) (changeLight { state = Just TOGGLE })
    publishLightSteps queue client time light
  when (m == "up_hold" || m == "up_press" || m == "brightness_move_up") $
    publishLight queue client (topic light  <> setT) (changeLight { brightnessStep = Just 30, transition = Just 2 })
  when (m == "down_hold" || m == "down_press" || m == "brightness_move_down") $
    publishLight queue client (topic light  <> setT) (changeLight { brightnessStep = Just (-30), transition = Just 2 })
