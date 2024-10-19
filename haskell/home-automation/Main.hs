{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.Thread.Delay
import Control.Exception (bracket)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import Data.Either (lefts)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.LocalTime
import HomeAutomation
import Network.MQTT.Client
import Network.MQTT.Topic
import Network.URI (URI)
import Network.URI.Static (staticURI)
import System.Exit (exitFailure)
import System.IO

type MqttState = Map Topic (ChangeLight, SystemTime)

-- | checks whether the light is on
isLightOn :: MqttState -> Light -> Bool
isLightOn mqttState light = fromMaybe False $ do
  curChangeLight <- Map.lookup (topic light) mqttState
  curState <- state (fst curChangeLight)
  pure (curState == ON)

publishLightSteps ::
  TVar MqttState ->
  TQueue Logger ->
  MQTTClient ->
  [ChangeLight] ->
  Light ->
  IO ()
publishLightSteps stateVar queue client steps light = do
  forM_ steps $ \step -> do
    publishLight stateVar queue client (topic light) step
    delay (maybe 0 ((* (1000 * 1000)) . fromIntegral) $ transition step)

publishLightStepsToggle ::
  TVar MqttState ->
  TQueue Logger ->
  MQTTClient ->
  TimeOfDay ->
  [Light] ->
  IO ()
publishLightStepsToggle stateVar queue client tod lights = do
  mqttState <- readTVarIO stateVar
  atomically
    . writeTQueue queue
    . LogMessage
    $ "Current state is: " <> T.pack (show mqttState)
  if any (isLightOn mqttState) lights
    then forM_ lights $ \light ->
      publishLight
        stateVar
        queue
        client
        (topic light)
        changeLight {state = Just OFF}
    else forM_ lights $ \light ->
      async (publishLightStepsON stateVar queue client tod light)

publishLightStepsIfOn ::
  TVar MqttState ->
  TQueue Logger ->
  MQTTClient ->
  TimeOfDay ->
  Light ->
  IO ()
publishLightStepsIfOn stateVar queue client tod light = do
  let (_, steps) = timeToColorTemp tod light
  mqttState <- readTVarIO stateVar
  atomically
    . writeTQueue queue
    . LogMessage
    $ "Current state is: " <> T.pack (show mqttState)
  if isLightOn mqttState light
    then
      publishLightSteps
        stateVar
        queue
        client
        steps
        light
    else
      atomically
        . writeTQueue queue
        . LogMessage
        $ "Won't publish light, "
          <> unTopic (topic light)
          <> " because it is off"

-- send multiple signals to bulb according to step duration. This is
-- necessary because some bulbs have an upper transition time
publishLightStepsON ::
  TVar MqttState ->
  TQueue Logger ->
  MQTTClient ->
  TimeOfDay ->
  Light ->
  IO ()
publishLightStepsON stateVar queue client tod light = do
  let (startTemp, steps) = timeToColorTemp tod light
  publishLight
    stateVar
    queue
    client
    (topic light)
    startTemp
      { state = Just ON,
        brightness = Just (lowestBrightness light)
      }
  delay 1000
  publishLight
    stateVar
    queue
    client
    (topic light)
    startTemp
      { state = Just ON,
        brightness = Just (highestBrightness light),
        transition = Just 3
      }
  delay (3 * 1000 * 1000)
  publishLightSteps stateVar queue client steps light

-- publish a mqtt message to a light
publishLight ::
  TVar MqttState ->
  TQueue Logger ->
  MQTTClient ->
  Topic ->
  ChangeLight ->
  IO ()
publishLight stateVar queue client topic light = do
  atomically
    . writeTQueue queue
    . LogMessage
    $ "Changing light for "
      <> unTopic topic
      <> ": "
      <> T.decodeUtf8 (BS.toStrict (encode light))
  publish client (topic <> setT) (encode light) False
  void . async $ do
    before <- getSystemTime
    delay (2 * 1000 * 1000)
    atomically $ do
      state <- readTVar stateVar
      when (maybe False ((<= before) . snd) $ Map.lookup topic state) $ do
        let newState = Map.delete topic state
        writeTVar stateVar newState

getCurrentTimeOfDay :: IO TimeOfDay
getCurrentTimeOfDay = do
  time <- getCurrentTime
  timeZone <- getTimeZone time
  pure $ localTimeOfDay $ utcToLocalTime timeZone time

colorTempRunner ::
  TVar MqttState ->
  TQueue Logger ->
  MQTTClient ->
  [Light] ->
  IO ()
colorTempRunner stateVar queue client lights = do
  forever $ do
    timeBeforeSleep <- getCurrentTimeOfDay
    let seconds =
          minimum $
            map
              (timeUntil timeBeforeSleep)
              [dayTimeStart, nightTimeStart]
    atomically
      . writeTQueue queue
      . LogMessage
      $ "It is "
        <> T.pack (show timeBeforeSleep)
        <> ". Sleeping for"
        <> T.pack (show seconds)
        <> " seconds."
    delay (fromIntegral $ diffTimeToMicroseconds seconds)
    lightChangeTime <- getCurrentTimeOfDay
    atomically
      . writeTQueue queue
      . LogMessage
      $ "It is "
        <> T.pack (show lightChangeTime)
        <> ". Starting color transition for lights: "
        <> T.pack (show lights)
    forM_ lights $
      \light ->
        async
          ( publishLightStepsIfOn
              stateVar
              queue
              client
              lightChangeTime
              light
          )

mosquittoURI :: URI
mosquittoURI = $$(staticURI "mqtt://localhost")

runMqtt :: TVar MqttState -> TQueue Logger -> IO ()
runMqtt stateVar queue =
  bracket
    ( connectURI
        mqttConfig
          { _msgCB =
              SimpleCallback
                (handleMessage stateVar queue)
          }
        mosquittoURI
    )
    ( \mc -> do
        stopClient mc
        atomically $ writeTQueue queue StopLogging
    )
    ( \mc -> do
        (lefts -> errs, _) <-
          subscribe
            mc
            ( map
                ((,subOptions) . toFilter)
                [ "zigbee2mqtt/switch/kitchen/action",
                  "zigbee2mqtt/switch/livingRoom/action",
                  "zigbee2mqtt/switch/hallway/action",
                  "zigbee2mqtt/switch/bedroom/action",
                  "zigbee2mqtt/bridge/event",
                  kitchenOneT,
                  kitchenTwoT,
                  kitchenThreeT,
                  livingRoomT,
                  bedroomT,
                  hallwayT,
                  dinerRoomT
                ]
            )
            []
        unless (null errs) $
          atomically . writeTQueue queue . StopLoggingWithFailure $
            "Got errors wile subscribing: " <> T.intercalate ", " (map (T.pack . show) errs)
        colorTempRunner
          stateVar
          queue
          mc
          [ livingRoom,
            kitchenOne,
            kitchenTwo,
            kitchenThree,
            hallway,
            dinerRoom,
            bedroom
          ]
        waitForClient mc
    )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  queue <- atomically newTQueue
  stateVar <- newTVarIO Map.empty
  withAsync (runMqtt stateVar queue) $ \_ -> do
    logger queue
    putStrLn "Terminating program"

data Logger
  = LogMessage T.Text
  | StopLogging
  | StopLoggingWithFailure T.Text

logger :: TQueue Logger -> IO ()
logger queue =
  atomically (readTQueue queue) >>= \case
    LogMessage msg -> do
      T.putStrLn msg
      logger queue
    StopLogging -> pure ()
    StopLoggingWithFailure msg -> do
      T.putStrLn msg
      exitFailure

handleMessage ::
  TVar MqttState ->
  TQueue Logger ->
  MQTTClient ->
  Topic ->
  ByteString ->
  [Property] ->
  IO ()
handleMessage mqttState queue client reasonTopic m _ = do
  atomically . writeTQueue queue . LogMessage $
    "Handle message of topic: " <> unTopic reasonTopic
  time <- getCurrentTimeOfDay
  when (reasonTopic == "zigbee2mqtt/switch/kitchen/action") $
    handleSwitch
      mqttState
      queue
      [dinerRoom, kitchenOne, kitchenTwo, kitchenThree]
      client
      m
  when (reasonTopic == "zigbee2mqtt/switch/livingRoom/action") $
    handleSwitch mqttState queue [livingRoom] client m
  when (reasonTopic == "zigbee2mqtt/switch/hallway/action") $
    handleSwitch mqttState queue [hallway] client m
  when (reasonTopic == "zigbee2mqtt/switch/bedroom/action") $
    handleSwitch mqttState queue [bedroom] client m
  when (reasonTopic == "zigbee2mqtt/bridge/event") $
    case decode m of
      Nothing ->
        atomically
          . writeTQueue queue
          . LogMessage
          $ "Parsing of bridge event failed: "
            <> T.decodeUtf8 (BS.toStrict m)
      Just
        BridgeEvent
          { eventKind =
              BridgeEventDeviceAnnounce,
            friendlyName = Just (mkTopic -> Just name)
          } -> do
          atomically . writeTQueue queue . LogMessage $
            "got Event with name: " <> unTopic name
          forM_ (matchLight name) $ \light -> do
            atomically . writeTQueue queue $ LogMessage "matched light"
            publishLightStepsON mqttState queue client time light
      Just ev ->
        atomically . writeTQueue queue . LogMessage $
          "Ignoring event: " <> T.pack (show ev)
  when (match "zigbee2mqtt/lights/#" reasonTopic) $
    case decode m of
      Nothing ->
        atomically
          . writeTQueue queue
          . LogMessage
          $ "Parsing of light failed: " <> T.decodeUtf8 (BS.toStrict m)
      Just changedLight -> do
        sysTime <- getSystemTime
        newLight <- atomically (setState sysTime mqttState queue reasonTopic changedLight)
        case (newLight, matchLight reasonTopic) of
          (True, Just light) -> publishLightStepsON mqttState queue client time light
          _ -> pure ()

-- | Handles which light one switch handles
handleSwitch ::
  TVar MqttState ->
  TQueue Logger ->
  [Light] ->
  MQTTClient ->
  ByteString ->
  IO ()
handleSwitch mqttState queue lights client m = do
  time <- getCurrentTimeOfDay
  when (m == "on_press_release" || m == "on") $ do
    publishLightStepsToggle mqttState queue client time lights
  when (m == "up_hold" || m == "up_press" || m == "brightness_move_up") $
    forM_ lights $
      \light ->
        publishLight
          mqttState
          queue
          client
          (topic light)
          (changeLight {brightnessStep = Just 30, transition = Just 2})
  when
    ( m == "down_hold"
        || m == "down_press"
        || m == "brightness_move_down"
    )
    $ forM_ lights
    $ \light ->
      publishLight
        mqttState
        queue
        client
        (topic light)
        (changeLight {brightnessStep = Just (-30), transition = Just 2})

-- | Sets the state of lights
setState ::
  SystemTime ->
  TVar MqttState ->
  TQueue Logger ->
  Topic ->
  ChangeLight ->
  STM Bool
setState sysTime stateVar queue topic newChangeLight = do
  mqttState <- readTVar stateVar
  let (res, newState) = Map.alterF updateState topic mqttState
  writeTVar stateVar newState
  writeTQueue queue
    . LogMessage
    $ "Changed state of topic "
      <> unTopic topic
      <> " to: "
      <> T.pack (show newChangeLight)
  pure res
  where
    updateState v =
      case v of
        Nothing -> (True, Just (newChangeLight, sysTime))
        Just (old, _) ->
          let updateField fieldAccessor =
                fieldAccessor newChangeLight <|> fieldAccessor old
           in ( False,
                Just
                  ( ChangeLight
                      { state = updateField state,
                        brightness = updateField brightness,
                        colorTemp = updateField colorTemp,
                        colorTempMove = updateField colorTempMove,
                        colorTempStep = updateField colorTempStep,
                        colorTempStartup = updateField colorTempStartup,
                        color = updateField color,
                        transition = updateField transition,
                        brightnessMove = updateField brightnessMove,
                        brightnessStep = updateField brightnessStep,
                        huePowerOnBehavior = updateField huePowerOnBehavior,
                        huePowerOnBrightness = updateField huePowerOnBrightness,
                        huePowerOnColorTemperature = updateField huePowerOnColorTemperature,
                        huePowerOnColor = updateField huePowerOnColor
                      },
                    sysTime
                  )
              )
