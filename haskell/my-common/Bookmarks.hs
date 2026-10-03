{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bookmarks where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import qualified Data.Text as T
import Protolude
import Turtle hiding (arg, err, stderr)

data Bookmarks = Bookmarks
  { normal :: [(Text, Text)],
    private :: [(Text, Text)],
    work :: [(Text, Text)]
  }
  deriving (Show)

instance FromJSON Bookmarks where
  parseJSON = \case
    Object v -> do
      normal <- v .: "normal" >>= toKeyValueText
      private <- v .: "private" >>= toKeyValueText
      work <- v .: "work" >>= toKeyValueText
      pure Bookmarks {..}
    other -> typeMismatch "object" other
    where
      toKeyValueText = \case
        Object obj ->
          forM (KM.toList obj) $ \(key, value) ->
            (K.toText key,) <$> parseJSON value
        other -> typeMismatch "object" other

-- | check the base of the url matches one of the bases of the given url
urlMatches :: Text -> [(Text, Text)] -> Bool
urlMatches url =
  any ((getBaseUrl url ==) . getBaseUrl . snd)

-- | strip prefix if it exists
tryStripPrefix :: Text -> Text -> Text
tryStripPrefix prfx str = fromMaybe str $ T.stripPrefix prfx str

getBaseUrl :: Text -> Text
getBaseUrl url =
  let noPrefixUrl = tryStripPrefix "www." . tryStripPrefix "http://" $ tryStripPrefix "https://" url
      noSuffixUrl = fromMaybe noPrefixUrl . head $ T.splitOn "/" noPrefixUrl
   in if all (\c -> isNumber c || c == ':' || c == '.') $ T.unpack noSuffixUrl
        then noSuffixUrl
        else case reverse . T.splitOn "." $ noSuffixUrl of
          (ext : base : _) -> base <> "." <> ext
          _ -> noSuffixUrl

getBookmarks :: IO Bookmarks
getBookmarks = do
  bookmarksJson <- BSL.readFile "/home/florian/.my-bookmarks.json"
  case eitherDecode bookmarksJson of
    Left err -> do
      hPutStrLn stderr $ "Error while parsing bookmarks: " <> err
      pure
        Bookmarks
          { normal = [],
            private = [],
            work = []
          }
    Right v -> pure v

isBookmark :: (Bookmarks -> [(Text, Text)]) -> IO ()
isBookmark selectBookmarks = do
  [arg] <- getArgs
  bookmarks <- selectBookmarks <$> getBookmarks
  if urlMatches (T.pack arg) bookmarks
    then exit ExitSuccess
    else exit (ExitFailure 1)
