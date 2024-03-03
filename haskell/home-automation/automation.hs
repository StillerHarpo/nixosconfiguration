{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
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

data Move = Step Int | Stop
  deriving (Generic, Show)

data Light = Light
  { state :: Maybe State,
    brightness :: Maybe Int,
    colorTemp :: Maybe ColorTemp,
    colorTempStartup :: Maybe ColorTempStartup,
    color :: Maybe Color,
    transition :: Maybe Int,
    brightnessMove :: Maybe Move,
    brightnessStep :: Maybe Int,
    colorTempMove :: Maybe Move,
    colorTempStep :: Maybe Int
  }
  deriving (Generic, Show)

data Status = Started | Successful | Failed
  deriving (Generic, Show)

data Definition = Definition {
    model :: Maybe Text,
    vendor :: Maybe Text,
    description :: Maybe Text
  }
  deriving (Generic, Show)

data BridgeEvent = BridgeEvent
  { friendlyName :: Maybe Text,
    ieeeAddress :: Maybe Int,
    eventKind :: BridgeEventKind
  }
  deriving (Generic, Show)

data BridgeEventKind =
  BridgeEventDeviceJoined
  | BridgeEventDeviceInterview {
      status :: Maybe Status,
      definition :: Maybe Definition
                               }
  | BridgeEventDeviceLeave
  | BridgeEventDeviceAnnounce
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

instance ToJSON Move where
  toJSON = myGenericToJSON

instance FromJSON Move where
  parseJSON = myGenericParseJSON

instance ToJSON Light where
  toJSON = myGenericToJSON

instance FromJSON Light where
  parseJSON = myGenericParseJSON

instance ToJSON Status where
  toJSON = myGenericToJSON

instance FromJSON Status where
  parseJSON = myGenericParseJSON

instance ToJSON Definition where
  toJSON = myGenericToJSON

instance FromJSON Definition where
  parseJSON = myGenericParseJSON

instance ToJSON BridgeEvent where
  toJSON BridgeEvent{..} =
    let commenData = [
            "friendly_name" .= friendlyName,
            "ieee_address" .= ieeeAddress
          ]
    in case eventKind of
      BridgeEventDeviceJoined ->
        object [
         "type" .= ("device_joined" :: Text),
         "data" .= object commenData
        ]
      BridgeEventDeviceInterview{..} ->
        object [
         "type" .= ("device_joined" :: Text),
         "data" .= object (
             commenData <> [ "status" .= status, "definition" .= definition ]
             )
        ]
      BridgeEventDeviceLeave ->
        object [
         "type" .= ("device_leave" :: Text),
         "data" .= object commenData
        ]
      BridgeEventDeviceAnnounce ->
        object [
         "type" .= ("device_announce" :: Text),
         "data" .= object commenData
        ]

instance FromJSON BridgeEvent where
  parseJSON = withObject "BridgeEvent" $ \v -> do
     data' <- v .: "data"
     friendlyName <- data' .:? "friendly_name"
     ieeeAddress <- data' .:? "ieee_address"
     eventKind <- (v .: "type" :: Parser Text) >>= \case
       "device_joined" -> pure BridgeEventDeviceJoined
       "device_interview" -> do
         status <- data' .:? "ieee_address"
         definition <- data' .:? "ieee_address"
         pure BridgeEventDeviceInterview{..}
       "device_leave" -> pure BridgeEventDeviceLeave
       "device_announce" -> pure BridgeEventDeviceAnnounce
     pure BridgeEvent{..}

light :: Light
light =
  Light
    { state = Nothing,
      brightness = Nothing,
      colorTemp = Nothing,
      colorTempStartup = Nothing,
      color = Nothing,
      transition = Nothing,
      brightnessMove = Nothing,
      brightnessStep = Nothing,
      colorTempMove = Nothing,
      colorTempStep = Nothing
    }

-- run Light over
puplishLightSteps :: DiffTime -> MQTTClient -> Topic -> IO ()
puplishLightSteps stepDuration client topic =
  forM_ (undefined :: [Light]) $ \light -> do
    publishLight client topic light
    threadDelay (diffTimeToSeconds stepDuration)

-- publish a mqtt message to a light
publishLight :: MQTTClient -> Topic -> Light -> IO ()
publishLight client topic light = do
  putStrLn $ "Changing light for " <> show topic
  publish client topic (encode light) False


diffTimeToSeconds :: DiffTime -> Int
diffTimeToSeconds = div 1000 . fromInteger . diffTimeToMicroseconds

-- Calculates a color temp based on the given time. At Night in should be a warm temp
-- and during the day it should be cold temp. Between night and day and vice versa is a transition
-- over 1 hour
timeToColorTemp :: TimeOfDay -> Light
timeToColorTemp t 
  | t >= dayTimeStart  && timeOfDayToTime t >= timeOfDayToTime t + duration =
    light {colorTemp = Just Coolest, transition = Just . diffTimeToSeconds $ timeOfDayToTime dayTimeStart + duration - timeOfDayToTime t}
  | timeOfDayToTime t >= timeOfDayToTime dayTimeStart + duration && t <= nightTimeStart = light {colorTemp = Just Coolest}
  | t >= nightTimeStart  && timeOfDayToTime t >= timeOfDayToTime t + duration =
    light {colorTemp = Just Warmest, transition = Just . diffTimeToSeconds $ timeOfDayToTime nightTimeStart + duration - timeOfDayToTime t}
  | otherwise = light {colorTemp = Just Warmest }

duration :: DiffTime
duration = secondsToDiffTime (60 * 60)

nightTimeStart :: TimeOfDay
nightTimeStart = hour 19

dayTimeStart :: TimeOfDay
dayTimeStart = hour 8

-- calculate the next time we want to shift colors
getNextTimeColorShift :: TimeOfDay -> TimeOfDay
getNextTimeColorShift t 
  | timeUntil t nightTimeStart < timeUntil t dayTimeStart = nightTimeStart
  | otherwise = dayTimeStart

getCurrentTimeOfDay :: IO TimeOfDay
getCurrentTimeOfDay = do
  time <- getCurrentTime
  timeZone <- getTimeZone time
  pure $ localTimeOfDay $ utcToLocalTime timeZone time

colorTempRunner :: MQTTClient -> [Topic] -> IO ()
colorTempRunner client topics = do
  forever $ do
    timeBeforeSleep <- getCurrentTimeOfDay
    let seconds = minimum $ map (timeUntil timeBeforeSleep) [dayTimeStart, nightTimeStart]
    threadDelay (fromIntegral $ diffTimeToMicroseconds seconds)
    lightChangeTime <- getCurrentTimeOfDay
    forM_ topics $ \topic -> publishLight client topic (timeToColorTemp $ getNextTimeColorShift lightChangeTime)

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

zigbee2mqttT, lightsT, kitchenT, livingRoomT, kitchenOneT, kitchenTwoT, kitchenThreeT, allT, setT, availabilityT :: Topic
zigbee2mqttT = "zigbee2mqtt"
lightsT = zigbee2mqttT <> "lights"
kitchenT = lightsT <> "kitchen"
kitchenOneT = kitchenT <> "one"
kitchenTwoT = kitchenT <> "two"
kitchenThreeT = kitchenT <> "three"
livingRoomT = lightsT  <> "livingRoom"
allT = "all"
setT = "set"
availabilityT = "availability"

main :: IO ()
main = do
  mc <- connectURI mqttConfig {_msgCB = SimpleCallback handleMessage} mosquittoURI
  subscribe mc (map ((, subOptions) . toFilter) ["zigbee2mqtt/switch/action", kitchenOneT, kitchenTwoT, kitchenThreeT, livingRoomT]) []
  forkIO $ colorTempRunner mc [livingRoomT <> setT, kitchenT <> setT]
  waitForClient mc -- wait for the client to disconnect

handleMessage :: MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
handleMessage client reasonTopic m _ = do
  putStrLn $ "Handle message of topic" <> show reasonTopic
  time <- getCurrentTimeOfDay
  when (m == "on_press_release") $ do
    publishLight client (livingRoomT <> setT) (light { state = Just TOGGLE })
  when (m == "off_press_release") $ do
    publishLight client (kitchenT <> allT <> setT) (light { state = Just TOGGLE })
    publishLight client (kitchenT <> allT <> setT) (timeToColorTemp time)
  when (m == "up_hold" || m == "up_press") $ 
    publishLight client (lightsT  <> allT <> setT) (light { brightnessStep = Just 30, transition = Just 2 })
  when (m == "down_hold" || m == "up_press") $ 
    publishLight client (lightsT  <> allT <> setT) (light { brightnessStep = Just (-30), transition = Just 2 })
  case decode m of
    Nothing -> putStrLn $ "Parsing of event not implemented: " <> show m
    Just BridgeEvent{eventKind = BridgeEventDeviceJoined, friendlyName = Just (mkTopic -> Just name)} -> do
      publishLight client (zigbee2mqttT <> name <> setT) (timeToColorTemp time)
    Just ev -> putStrLn $ "Ignoring event: " <> show ev
