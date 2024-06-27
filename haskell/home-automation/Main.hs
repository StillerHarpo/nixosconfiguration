{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
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

-- send multiple signals to bulb according to step duration. This is
-- necessary because some bulbs have an upper transition time
publishLightSteps :: MQTTClient -> TimeOfDay -> Light -> IO ()
publishLightSteps client tod light = do
  let (startTemp, steps) = timeToColorTemp tod light
  publishLight client (topic light <> setT) startTemp
  forM_ steps $ \changeLight -> do
    publishLight client (topic light <> setT) changeLight
    threadDelay (maybe 0 (* (1000 * 1000)) $ transition changeLight)

-- publish a mqtt message to a light
publishLight :: MQTTClient -> Topic -> ChangeLight -> IO ()
publishLight client topic light = do
  putStrLn $ "Changing light for " <> show topic <> ": " <> show (encode light)
  publish client topic (encode light) False

getCurrentTimeOfDay :: IO TimeOfDay
getCurrentTimeOfDay = do
  time <- getCurrentTime
  timeZone <- getTimeZone time
  pure $ localTimeOfDay $ utcToLocalTime timeZone time

colorTempRunner :: MQTTClient -> [Light] -> IO ()
colorTempRunner client lights = do
  forever $ do
    timeBeforeSleep <- getCurrentTimeOfDay
    let seconds = minimum $ map (timeUntil timeBeforeSleep) [dayTimeStart, nightTimeStart]
    threadDelay (fromIntegral $ diffTimeToMicroseconds seconds)
    lightChangeTime <- getCurrentTimeOfDay
    forM_ lights $ publishLightSteps client lightChangeTime

mosquittoURI :: URI
mosquittoURI = $$(staticURI "mqtt://localhost")

main :: IO ()
main = do
  mc <- connectURI mqttConfig {_msgCB = SimpleCallback handleMessage} mosquittoURI
  subscribe mc (map ((, subOptions) . toFilter) [
                   "zigbee2mqtt/switch/kitchen/action",
                   "zigbee2mqtt/switch/livingRoom/action",
                   "zigbee2mqtt/switch/hallway/action",
                   "zigbee2mqtt/switch/bedroom/action",
                   "zigbee2mqtt/bridge/event",
                   kitchenOneT, kitchenTwoT, kitchenThreeT, livingRoomT, bedroomT, hallwayT, dinerRoomT]) []
  forkIO $ colorTempRunner mc [livingRoom, kitchen, hallway, dinerRoom, bedroom]
  waitForClient mc -- wait for the client to disconnect

handleMessage :: MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
handleMessage client reasonTopic m _ = do
  putStrLn $ "Handle message of topic" <> show reasonTopic
  time <- getCurrentTimeOfDay
  when (reasonTopic == "zigbee2mqtt/switch/kitchen/action") $ handleSwitch kitchenAndDinerRoom client m
  when (reasonTopic == "zigbee2mqtt/switch/livingRoom/action") $ handleSwitch livingRoom client m
  when (reasonTopic == "zigbee2mqtt/switch/hallway/action") $ handleSwitch hallway client m
  when (reasonTopic == "zigbee2mqtt/switch/bedroom/action") $ handleSwitch bedroom client m
  when (reasonTopic == "zigbee2mqtt/bridge/event") $
    case decode m of
      Nothing -> putStrLn $ "Parsing of bridge event failed: " <> show m
      Just BridgeEvent{eventKind = BridgeEventDeviceAnnounce, friendlyName = Just (mkTopic -> Just name)} -> do
        T.putStrLn $ "got Event with name: " <> unTopic name
        forM_ (matchLight name) $ \light -> do
          T.putStrLn "matched light"
          publishLight client (name <> setT) (changeLight { state = Just ON })
          publishLightSteps client time light
      Just ev -> putStrLn $ "Ignoring event: " <> show ev

-- | Handles which light one switch handles
handleSwitch ::  Light -> MQTTClient -> ByteString -> IO ()
handleSwitch light client m = do
  time <- getCurrentTimeOfDay
  when (m == "on_press_release" || m == "on") $ do
    publishLight client (topic light <> setT) (changeLight { state = Just TOGGLE })
    publishLightSteps client time light
  when (m == "up_hold" || m == "up_press" || m == "brightness_move_up") $
    publishLight client (topic light  <> setT) (changeLight { brightnessStep = Just 30, transition = Just 2 })
  when (m == "down_hold" || m == "down_press" || m == "brightness_move_down") $
    publishLight client (topic light  <> setT) (changeLight { brightnessStep = Just (-30), transition = Just 2 })
