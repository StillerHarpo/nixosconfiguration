{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module HomeAutomation where
import GHC.Generics
import Network.MQTT.Topic
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types (Parser)

data State = ON | OFF | TOGGLE
  deriving (Generic, Show, Eq)

data ColorTemp = Exact Int | Coolest | Cool | Neutral | Warm | Warmest
  deriving (Generic, Show, Eq)

data ColorTempStartup = ColorTemp ColorTemp | Previous
  deriving (Generic, Show, Eq)

data Color = ColorXY {x :: Int, y :: Int} | ColorRGB {red :: Int, green :: Int, blue :: Int}
  deriving (Generic, Show, Eq)

data Move = Step Int | Stop
  deriving (Generic, Show, Eq)

data ChangeLight = ChangeLight
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
  deriving (Generic, Show, Eq)

data Light = Light
  { topic :: Topic,
    maxStep :: Maybe Int,
    lowestColorTemp :: Int,
    highestColorTemp :: Int
  }

data Direction a = Up a | Down a
   deriving (Show, Eq)

data LightStepper = LightStepper
  { colorTempStart :: Int,
    steps :: Int,
    colorTempChange :: Int,
    stepDuration :: DiffTime
   } deriving (Show, Eq)

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
    ieeeAddress :: Maybe Text,
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

instance ToJSON ChangeLight where
  toJSON = myGenericToJSON

instance FromJSON ChangeLight where
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
         status <- data' .:? "status"
         definition <- data' .:? "definition"
         pure BridgeEventDeviceInterview{..}
       "device_leave" -> pure BridgeEventDeviceLeave
       "device_announce" -> pure BridgeEventDeviceAnnounce
     pure BridgeEvent{..}

changeLight :: ChangeLight
changeLight =
  ChangeLight
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

-- | Get LightStepper to get from lowest color temp to highest (or vice versa)
getLightStepper :: Direction DiffTime -> Light -> LightStepper
getLightStepper td lc =
  LightStepper {
    stepDuration = maybe dt (secondsToDiffTime . fromIntegral) $ maxStep lc,
    ..
    }
  where
    steps = maybe 1 (diffTimeToSeconds dt `div`) (maxStep lc)
    (dt, colorTempStart, colorTempChange) =
      case td of
        Up dt -> let
          startTemp =
            round $
              transformPointToOtherRange
                0 (fromIntegral $ diffTimeToPicoseconds duration) (fromIntegral . diffTimeToPicoseconds $ duration - dt)
                (fromIntegral $ lowestColorTemp lc) (fromIntegral $ highestColorTemp lc)
          in (dt, startTemp, (highestColorTemp lc - startTemp) `div` steps)
        Down dt -> let
          startTemp =
            round $
              transformPointToOtherRange
                0 (fromIntegral $ diffTimeToPicoseconds duration) (fromIntegral $ diffTimeToPicoseconds dt)
                (fromIntegral $ lowestColorTemp lc) (fromIntegral $ highestColorTemp lc)
          in (dt, startTemp, (-1) * ((startTemp - lowestColorTemp lc) `div` steps))

changeLightSteps :: LightStepper -> (ChangeLight, [ChangeLight])
changeLightSteps stepper =
  (changeLight { colorTemp = Just . Exact $ colorTempStart stepper},
    map (\temp ->
           changeLight {
            colorTemp = Just . Exact $ temp + colorTempStart stepper,
            transition = Just (diffTimeToSeconds (stepDuration stepper))
            })
    . scanl1 (+) $ replicate (steps stepper) (colorTempChange stepper))

-- | cuts the list down to a fraction of to numbers by removing at the front
dropFraction :: (RealFrac a, Fractional a) => a -> a  -> [b] -> [b]
dropFraction x y xs = drop (round (fromIntegral (length xs - 1) / (x / y))) xs

-- | given an point on a range give the corresponding point on another range
-- where the ratio to its lower and upper bound is the same
transformPointToOtherRange :: Fractional a => a -> a -> a -> a -> a -> a
transformPointToOtherRange lower upper point lower' upper' = (upper' - lower') * ((point - lower) / (upper - lower)) + lower'

-- | Calculates a color temp based on the given time. At Night in should be a warm temp
-- and during the day it should be cold temp. Between night and day and vice versa is a transition
-- over 1 hour
timeToColorTemp :: TimeOfDay -> Light ->  (ChangeLight, [ChangeLight])
timeToColorTemp t light
  | t >= dayTimeStart && timeOfDayToTime t <= timeOfDayToTime dayTimeStart + duration =
    let
      restDuration = timeOfDayToTime dayTimeStart + duration - timeOfDayToTime t
      stepper = getLightStepper (Down restDuration) light
    in changeLightSteps stepper
  | timeOfDayToTime t > timeOfDayToTime dayTimeStart + duration && t < nightTimeStart =
    (changeLight {colorTemp = Just Coolest}, [])
  | t >= nightTimeStart  && timeOfDayToTime t <= timeOfDayToTime nightTimeStart + duration = let
      restDuration = timeOfDayToTime nightTimeStart + duration - timeOfDayToTime t
      stepper = getLightStepper (Up restDuration) light
    in changeLightSteps stepper
  | otherwise = (changeLight {colorTemp = Just Warmest }, [])

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

timeUntil :: TimeOfDay -> TimeOfDay -> DiffTime
timeUntil from to = if t > 0 then t else timeOfDayToTime (TimeOfDay 24 0 0) + t
  where
    t = timeOfDayToTime to - timeOfDayToTime from

hour :: Int -> TimeOfDay
hour h = TimeOfDay h 0 0

diffTimeToMicroseconds :: DiffTime -> Integer
diffTimeToMicroseconds = (`div` 1000) . (`div` 1000) . diffTimeToPicoseconds

diffTimeToSeconds :: DiffTime -> Int
diffTimeToSeconds = (`div` 1000) . (`div` 1000) . fromInteger . diffTimeToMicroseconds

zigbee2mqttT, lightsT, kitchenT, livingRoomT, kitchenOneT, kitchenTwoT, kitchenThreeT, allT, setT, availabilityT :: Topic
zigbee2mqttT = "zigbee2mqtt"
lightsT = zigbee2mqttT <> "lights"
kitchenT = lightsT <> "kitchen"
kitchenOneT = kitchenT <> "one"
kitchenTwoT = kitchenT <> "two"
kitchenThreeT = kitchenT <> "three"
dinerRoomT = lightsT  <> "dinerRoom"
kitchenAndDinerRoomT = lightsT  <> "kitchenAndDinerRoom"
livingRoomT = lightsT  <> "livingRoom"
hallwayT = lightsT  <> "hallway"
bedroomT = lightsT  <> "bedroom"
allT = "all"
setT = "set"
availabilityT = "availability"

livingRoom :: Light
livingRoom = Light {
  topic = livingRoomT,
  maxStep = Nothing,
  lowestColorTemp = 150,
  highestColorTemp = 500
  }

mkIkeaLight :: Topic -> Light
mkIkeaLight light = Light {
  topic = light,
  maxStep = Nothing,
  lowestColorTemp = 153,
  highestColorTemp = 500
  }

kitchen :: Light
kitchen = mkIkeaLight (kitchenT <> allT)

dinerRoom :: Light
dinerRoom = mkIkeaLight dinerRoomT

kitchenAndDinerRoom :: Light
kitchenAndDinerRoom = mkIkeaLight kitchenAndDinerRoomT

hallway :: Light
hallway = mkIkeaLight hallwayT

bedroom :: Light
bedroom = mkIkeaLight bedroomT

-- TODO
matchLight :: Topic -> Maybe Light
matchLight t
  | t == topic kitchen = Just kitchen
  | t == topic livingRoom = Just livingRoom
  | otherwise = Nothing
