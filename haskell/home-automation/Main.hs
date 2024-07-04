{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- send multiple signals to bulb according to step duration. This is
-- necessary because some bulbs have an upper transition time
publishLightSteps :: MVar () -> MQTTClient -> TimeOfDay -> Light -> IO ()
publishLightSteps lock client tod light = do
  let (startTemp, steps) = timeToColorTemp tod light
  publishLight lock client (topic light <> setT) startTemp
  forM_ steps $ \changeLight -> do
    publishLight lock client (topic light <> setT) changeLight
    threadDelay (maybe 0 (* (1000 * 1000)) $ transition changeLight)

-- publish a mqtt message to a light
publishLight :: MVar () -> MQTTClient -> Topic -> ChangeLight -> IO ()
publishLight lock client topic light = do
  asyncLog lock $ "Changing light for " <> unTopic topic <> ": " <> T.decodeUtf8 (BS.toStrict (encode light))
  publish client topic (encode light) False

getCurrentTimeOfDay :: IO TimeOfDay
getCurrentTimeOfDay = do
  time <- getCurrentTime
  timeZone <- getTimeZone time
  pure $ localTimeOfDay $ utcToLocalTime timeZone time

colorTempRunner :: MVar () -> MQTTClient -> [Light] -> IO ()
colorTempRunner lock client lights = do
  forever $ do
    timeBeforeSleep <- getCurrentTimeOfDay
    let seconds = minimum $ map (timeUntil timeBeforeSleep) [dayTimeStart, nightTimeStart]
    threadDelay (fromIntegral $ diffTimeToMicroseconds seconds)
    lightChangeTime <- getCurrentTimeOfDay
    forM_ lights $ publishLightSteps lock client lightChangeTime

mosquittoURI :: URI
mosquittoURI = $$(staticURI "mqtt://localhost")

main :: IO ()
main = do
  lock <- newMVar ()
  bracket
    (connectURI mqttConfig {_msgCB = SimpleCallback (handleMessage lock)} mosquittoURI)
    (\mc -> do
        subscribe mc (map ((, subOptions) . toFilter) [
                   "zigbee2mqtt/switch/kitchen/action",
                   "zigbee2mqtt/switch/livingRoom/action",
                   "zigbee2mqtt/switch/hallway/action",
                   "zigbee2mqtt/switch/bedroom/action",
                   "zigbee2mqtt/bridge/event",
                   kitchenOneT, kitchenTwoT, kitchenThreeT, livingRoomT, bedroomT, hallwayT, dinerRoomT]) []
        colorTempRunner lock mc [livingRoom, kitchen, hallway, dinerRoom, bedroom]
        waitForClient mc)
    (\mc -> do
      asyncLog lock "Terminating program"
      stopClient mc)

asyncLog :: MVar () -> T.Text -> IO ()
asyncLog lock msg = withMVar lock $ \_ -> T.putStrLn msg 

handleMessage :: MVar () -> MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
handleMessage lock client reasonTopic m _ = do
  asyncLog lock $ "Handle message of topic" <> unTopic reasonTopic
  time <- getCurrentTimeOfDay
  when (reasonTopic == "zigbee2mqtt/switch/kitchen/action") $ handleSwitch lock kitchenAndDinerRoom client m
  when (reasonTopic == "zigbee2mqtt/switch/livingRoom/action") $ handleSwitch lock livingRoom client m
  when (reasonTopic == "zigbee2mqtt/switch/hallway/action") $ handleSwitch lock hallway client m
  when (reasonTopic == "zigbee2mqtt/switch/bedroom/action") $ handleSwitch lock bedroom client m
  when (reasonTopic == "zigbee2mqtt/bridge/event") $
    case decode m of
      Nothing -> asyncLog lock $ "Parsing of bridge event failed: " <> T.decodeUtf8 (BS.toStrict m)
      Just BridgeEvent{eventKind = BridgeEventDeviceAnnounce, friendlyName = Just (mkTopic -> Just name)} -> do
        asyncLog lock $ "got Event with name: " <> unTopic name
        forM_ (matchLight name) $ \light -> do
          asyncLog lock "matched light"
          publishLight lock client (name <> setT) (changeLight { state = Just ON })
          publishLightSteps lock client time light
      Just ev -> asyncLog lock $ "Ignoring event: " <> T.pack (show ev)

-- | Handles which light one switch handles
handleSwitch :: MVar () -> Light -> MQTTClient -> ByteString -> IO ()
handleSwitch lock light client m = do
  time <- getCurrentTimeOfDay
  when (m == "on_press_release" || m == "on") $ do
    publishLight lock client (topic light <> setT) (changeLight { state = Just TOGGLE })
    publishLightSteps lock client time light
  when (m == "up_hold" || m == "up_press" || m == "brightness_move_up") $
    publishLight lock client (topic light  <> setT) (changeLight { brightnessStep = Just 30, transition = Just 2 })
  when (m == "down_hold" || m == "down_press" || m == "brightness_move_down") $
    publishLight lock client (topic light  <> setT) (changeLight { brightnessStep = Just (-30), transition = Just 2 })
