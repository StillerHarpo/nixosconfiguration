{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Tests where

import Bookmarks
import Data.Time.Clock
import Data.Time.LocalTime
import HomeAutomation
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ testGroup
          "automation"
          [ testCase "timeUntil works" $
              diffTimeToSeconds (timeUntil (hour 6) ((hour 6) {todSec = 10}))
                @?= 10,
            testCase "timeUntil works over midnight" $
              diffTimeToSeconds (timeUntil (hour 23) (hour 1))
                @?= (60 * 60 * 2),
            testCase "dropFraction works on empty list" $
              dropFraction @Double 2 1 ([] :: [Int]) @?= [],
            testCase "dropFraction works for 1/1" $
              dropFraction @Double 1 1 [1, 2, 3, 4] @?= [4 :: Integer],
            testCase "dropFraction works for 1000/999" $
              dropFraction @Double 1000 999 [1, 2, 3, 4] @?= [4 :: Integer],
            testCase "dropFraction works 1000/1" $
              dropFraction @Double 1000 1 [1, 2, 3, 4] @?= [1 :: Integer, 2, 3, 4],
            testCase "dropFraction works 2/1" $
              dropFraction @Double 2 1 [1, 2, 3, 4] @?= [3 :: Integer, 4],
            testCase "transformPointToOtherRange works" $
              transformPointToOtherRange 2 4 3 7 8 @?= (7.5 :: Double),
            testCase "getLightStepper works" $
              getLightStepper (Up duration) livingRoom
                @?= LightStepper
                  { colorTempStart = 150,
                    colorTempChange = 350,
                    stepDuration = secondsToDiffTime (60 * 60),
                    steps = 1
                  },
            testCase "changeLightSteps works for simple step" $
              let lightStepper =
                    LightStepper
                      { colorTempStart = 50,
                        colorTempChange = 100,
                        stepDuration = secondsToDiffTime (60 * 60),
                        steps = 1
                      }
                  res =
                    ( changeLight
                        { colorTemp = Just (Exact 50)
                        },
                      [ changeLight
                          { colorTemp = Just (Exact 150),
                            transition = Just (60 * 60)
                          }
                      ]
                    )
               in changeLightSteps lightStepper @?= res,
            testCase "kitchen at the middle of night" $
              timeToColorTemp (TimeOfDay 2 22 22) kitchen30
                @?= (changeLight {colorTemp = Just Warmest}, []),
            testCase "living room at middle of night" $
              timeToColorTemp (TimeOfDay 2 22 22) livingRoom
                @?= (changeLight {colorTemp = Just Warmest}, []),
            testCase "kitchen at middle of day" $
              timeToColorTemp (TimeOfDay 14 22 22) kitchen30
                @?= (changeLight {colorTemp = Just Coolest}, []),
            testCase "living room at middle of day" $
              timeToColorTemp (TimeOfDay 14 22 22) livingRoom
                @?= (changeLight {colorTemp = Just Coolest}, []),
            testCase "getLightStepper: living room at start of shift to day" $
              getLightStepper (Down $ secondsToDiffTime (60 * 60)) livingRoom
                @?= LightStepper
                  { colorTempStart = 500,
                    steps = 1,
                    colorTempChange = -350,
                    stepDuration = secondsToDiffTime (60 * 60)
                  },
            testCase "changeLightSteps: living room at start of shift to day" $
              let lightStepper =
                    LightStepper
                      { colorTempStart = 500,
                        steps = 1,
                        colorTempChange = -350,
                        stepDuration = secondsToDiffTime (60 * 60)
                      }
               in changeLightSteps lightStepper
                    @?= ( changeLight {colorTemp = Just (Exact 500)},
                          [changeLight {colorTemp = Just (Exact 150), transition = Just (60 * 60)}]
                        ),
            testCase "living room at start of shift to day" $
              timeToColorTemp (TimeOfDay 8 0 0) livingRoom
                @?= ( changeLight {colorTemp = Just (Exact 500)},
                      [changeLight {colorTemp = Just (Exact 150), transition = Just (60 * 60)}]
                    ),
            testCase "living room at start of shift to night" $
              timeToColorTemp (TimeOfDay 19 0 0) livingRoom
                @?= ( changeLight {colorTemp = Just (Exact 150)},
                      [changeLight {colorTemp = Just (Exact 500), transition = Just (60 * 60)}]
                    ),
            testCase "living room at middle of shift to night" $
              timeToColorTemp (TimeOfDay 19 30 0) livingRoom
                @?= ( changeLight {colorTemp = Just (Exact 325)},
                      [changeLight {colorTemp = Just (Exact 500), transition = Just (30 * 60)}]
                    ),
            testCase "living room at middle of shift to day" $
              timeToColorTemp (TimeOfDay 8 30 0) livingRoom
                @?= ( changeLight {colorTemp = Just (Exact 325)},
                      [changeLight {colorTemp = Just (Exact 150), transition = Just (30 * 60)}]
                    ),
            testCase "getLightStepper: kitchen 60 seconds before end of shift to night" $
              getLightStepper (Up (secondsToDiffTime 60)) kitchen30
                @?= LightStepper
                  { colorTempStart = 494,
                    steps = 2,
                    colorTempChange = 3,
                    stepDuration = secondsToDiffTime 30
                  },
            testCase "kitchen 60 seconds before end of shift to night" $
              timeToColorTemp (TimeOfDay 19 59 0) kitchen30
                @?= ( changeLight {colorTemp = Just (Exact 494)},
                      [ changeLight {colorTemp = Just (Exact 497), transition = Just 30},
                        changeLight {colorTemp = Just (Exact 500), transition = Just 30}
                      ]
                    ),
            testCase "kitchen 60 seconds before end of shift to day" $
              timeToColorTemp (TimeOfDay 8 59 0) kitchen30
                @?= ( changeLight {colorTemp = Just (Exact 159)},
                      [ changeLight {colorTemp = Just (Exact 156), transition = Just 30},
                        changeLight {colorTemp = Just (Exact 153), transition = Just 30}
                      ]
                    )
          ],
        testGroup
          "automation"
          [ testCase "http://www.aa.bb.cc/dd/ff" $
              getBaseUrl "http://www.aa.bb.cc/dd/"
                @?= "bb.cc",
            testCase "https://www.aa.bb.cc/dd/ff" $
              getBaseUrl "https://www.aa.bb.cc/dd/"
                @?= "bb.cc",
            testCase "www.aa.bb.cc/dd/ff" $
              getBaseUrl "www.aa.bb.cc/dd/"
                @?= "bb.cc",
            testCase "aa.bb" $
              getBaseUrl "aa.bb"
                @?= "aa.bb",
            testCase "http://192.168.178.11:1111/" $
              getBaseUrl "http://192.168.178.11:1111/" @?= "192.168.178.11:1111"
          ]
      ]
  where
    kitchen30 = kitchen {maxStep = Just 30}
