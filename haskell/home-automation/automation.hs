{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.LocalTime
import GHC.Generics
import Network.MQTT.Client
import Network.MQTT.Topic
import Network.URI (URI)
import Network.URI.Static (staticURI)

data State = ON | OFF | TOGGLE
  deriving (Generic, Show)

data ColorTemp = Exact Int | Coolest | Cool | Neutral | Warm | Warmest
  deriving (Generic, Show)

data ColorTempStartup = ColorTemp ColorTemp | Previous
  deriving (Generic, Show)

data Color = ColorXY {x :: Int, y :: Int} | ColorRGB {red :: Int, green :: Int, blue :: Int}
  deriving (Generic, Show)

data Light = Light
  { state :: Maybe State,
    brightness :: Maybe Int,
    colorTemp :: Maybe ColorTemp,
    colorTempStartup :: Maybe ColorTempStartup,
    color :: Maybe Color,
    transition :: Maybe Int
  }
  deriving (Generic, Show)

instance ToJSON State

instance FromJSON State

myJSONOptions =
  defaultOptions
    { sumEncoding = UntaggedValue,
      constructorTagModifier = camelTo2 '_',
      omitNothingFields = True,
      fieldLabelModifier = camelTo2 '_'
    }

myGenericToJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
myGenericToJSON = genericToJSON myJSONOptions

myGenericParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
myGenericParseJSON = genericParseJSON myJSONOptions

instance ToJSON ColorTemp where
  toJSON = myGenericToJSON

instance FromJSON ColorTemp where
  parseJSON = myGenericParseJSON

instance ToJSON ColorTempStartup where
  toJSON = myGenericToJSON

instance FromJSON ColorTempStartup where
  parseJSON = myGenericParseJSON

instance ToJSON Color where
  toJSON = myGenericToJSON

instance FromJSON Color where
  parseJSON = myGenericParseJSON

instance ToJSON Light where
  toJSON = myGenericToJSON

instance FromJSON Light where
  parseJSON = myGenericParseJSON

light :: Light
light =
  Light
    { state = Nothing,
      brightness = Nothing,
      colorTemp = Nothing,
      colorTempStartup = Nothing,
      color = Nothing,
      transition = Nothing
    }

-- publish a mqtt message to a light
publishLight :: MQTTClient -> Topic -> Light -> IO ()
publishLight client topic light = do
  putStrLn $ "Changing light for " <> show topic
  publish client topic (encode light) False

-- Calculates a color temp based on the given time. At Night in should be a warm temp
-- and during the day it should be cold temp. Between night and day and vice versa is a transition
-- over 1 hour
timeToColorTemp :: TimeOfDay -> Light
timeToColorTemp (TimeOfDay hour min _)
  | hour <= 20 && hour >= 8 = light {colorTemp = Just Coolest}
  | hour <= 7 || hour >= 21 = light {colorTemp = Just Warmest}
  | hour <= 8 && hour >= 7 = light {colorTemp = Just Coolest, transition = Just $ min * 60}
  | otherwise = light {colorTemp = Just Warmest, transition = Just $ min * 60}

getCurrentTimeOfDay :: IO TimeOfDay
getCurrentTimeOfDay = do
  time <- getCurrentTime
  timeZone <- getTimeZone time
  pure $ localTimeOfDay $ utcToLocalTime timeZone time

colorTempRunner :: MQTTClient -> [Topic] -> IO ()
colorTempRunner client topics = do
  forever $ do
    time <- getCurrentTimeOfDay
    let seconds = minimum $ map (timeUntil time . hour) [7, 21]
    threadDelay (fromIntegral $ diffTimeToMicroseconds seconds)
    forM_ topics $ \topic -> publishLight client topic (timeToColorTemp time) 

mosquittoURI :: URI
mosquittoURI = $$(staticURI "mqtt://localhost")

timeUntil :: TimeOfDay -> TimeOfDay -> DiffTime
timeUntil from to = if t > 0 then t else timeOfDayToTime (TimeOfDay 24 0 0) + t
  where
    t = timeOfDayToTime to - timeOfDayToTime from

hour :: Int -> TimeOfDay
hour h = TimeOfDay h 0 0

diffTimeToMicroseconds :: DiffTime -> Integer
diffTimeToMicroseconds = (`div` 1000) . (`div` 1000) . diffTimeToPicoseconds

lightsKitchenOne, lightsKitchenTwo, lightsKitchenThree, lightsKitchenAll, lightsLivingRoom, lightsAll :: Topic
lightsKitchenOne = "zigbee2mqtt/lights/kitchen/one/set"
lightsKitchenTwo = "zigbee2mqtt/lights/kitchen/two/set"
lightsKitchenThree = "zigbee2mqtt/lights/kitchen/three/set"
lightsKitchenAll = "zigbee2mqtt/lights/kitchen/all/set"
lightsLivingRoom = "zigbee2mqtt/lights/livingRoom/set"
lightsAll = "zigbee2mqtt/lights/all/set"

main :: IO ()
main = do
  mc <- connectURI mqttConfig {_msgCB = SimpleCallback handleMessage} mosquittoURI
  subscribe mc [("zigbee2mqtt/switch/action", subOptions)] []
  forkIO $ colorTempRunner mc [lightsAll]
  waitForClient mc -- wait the the client to disconnect


data BridgeEvent =
  BridgeEventDeviceJoined 
  { friendlyName :: Maybe Text,
    ieeeAddress :: Maybe Int
  }
  deriving (Generic, Show)

instance ToJSON BridgeEvent where
  toJSON bridgeEvent =
    case bridgeEvent of
      BridgeEventDeviceJoined{..} ->
        object [
         "type" .= ("device_joined" :: Text),
         "data" .= object [
            "friendly_name" .= friendlyName,
            "ieee_address" .= ieeeAddress
          ]
        ] 

instance FromJSON BridgeEvent where
  parseJSON = withObject "BridgeEvent" $ \v -> do
     data' <- v .: "data"
     (v .: "type" :: Parser Text) >>= \case
       "device_joined" -> do
         friendlyName <- data' .:? "friendly_name"
         ieeeAddress <- data' .:? "ieee_address"
         pure BridgeEventDeviceJoined{..}

-- {"level":"info","message":"MQTT publish: topic 'zigbee2mqtt/bridge/event', payload '{\"data\":{\"friendly_name\":\"lights/kitchen/one\",\"ieee_address\":\"0x680ae2fffe68e0d1\"},\"type\":\"device_announce\"}'"}
-- {"level":"info","message":"MQTT publish: topic 'zigbee2mqtt/switch/action', payload 'on_press'"}
handleMessage :: MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
handleMessage _ t m _ = do
  -- T.appendFile "/tmp/mqtt" (unTopic t <> "\n")
  LBS.appendFile "/tmp/mqtt" (m <> "\n")
  case decode m of
    Nothing -> pure ()
    Just light -> print $ state light
{-
zigbee2mqtt/bridge/config
zigbee2mqtt/bridge/devices
zigbee2mqtt/bridge/extensions
zigbee2mqtt/bridge/groups
zigbee2mqtt/bridge/info
zigbee2mqtt/bridge/logging
zigbee2mqtt/bridge/state
zigbee2mqtt/kitchen/all/availability
zigbee2mqtt/lights/all/availability
zigbee2mqtt/lights/all/set
zigbee2mqtt/switch
zigbee2mqtt/switch/action
-}
{-
down_press
down_press_release
off_press
off_press_release
on_press
on_press_release
up_press
up_press_release
-}
