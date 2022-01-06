{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (FilePath)
import Turtle hiding (stdin)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.IORef
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import Data.Semigroup(Max(..))
import Data.Foldable(traverse_)
import Data.Time.Format
import System.IO(hSetBuffering, BufferMode(..), stdin)
import Control.Monad.Catch
import Control.Monad.Extra
import Text.Read (readMaybe)

data Env = Env { counter :: IORef Int
               , scanner :: Text }

data CustomError =
  FileExistError FilePath
  | EncodingError
  | PnmsNotDevidableByError Int
  | NoScanner

instance Show CustomError where
  show = \case
     FileExistError filePath -> T.unpack $ format ("Error file"%fp%"exist") filePath
     EncodingError -> "encodig failed"
     PnmsNotDevidableByError n -> "internal error: number of pnsm should be devideable by " <> show n
     NoScanner -> "Couldn't find scanner"

instance Exception CustomError

type MyShell a = ReaderT Env Shell a

getCounter :: MyShell Int
getCounter = asks counter >>= liftIO . readIORef

incCounter :: MyShell ()
incCounter = asks counter >>= liftIO . flip modifyIORef' (+ 1)

getPnms :: Shell FilePath
getPnms = mfilter ((== Just "pnm") . snd . splitExtension) (ls ".")

pnmNumMatch :: Pattern Int
pnmNumMatch =  "./out" *> decimal <* ".pnm"

getPnmsSorted :: MonadIO m => m [FilePath]
getPnmsSorted = sortOn ((match pnmNumMatch <$>) . toText) getPnms

fileExistsError :: FilePath -> MyShell ()
fileExistsError file = do
  printf ("Error file"%fp%"exist"%"\n") file
  curCount <- getCounter
  printf ("counter was: "%d%"\n") curCount
  lift $ exit (ExitFailure 1)

today :: MonadIO m => m Text
today = T.pack . formatTime defaultTimeLocale "%d_%m_%Y" <$> date

getFileName :: MyShell Text
getFileName = do
  cur <- today
  curCount <- getCounter
  pure $ format ("input_"%d%"_"%s%".pdf") curCount cur

checkFile :: FilePath -> MyShell ()
checkFile file = do
  exists <- testfile file
  if exists
  then fileExistsError file
  else pure ()

convert :: [FilePath] -> MyShell ()
convert inputs = do
  fnT <- getFileName
  let fn = fromText fnT
  case traverse toText inputs of
        Right inputTs -> do
           checkFile fn
           printf ("Writing "%fp%"\n") fn
           procs "convert" (inputTs <> ["-quality", "100", fnT]) mempty
           incCounter
        _ -> throwM EncodingError

convertPictures :: MyShell ()
convertPictures = do
  pnm <- lift getPnms
  convert [pnm]
  rm pnm

convertMultiblePictures :: MyShell ()
convertMultiblePictures = do
  pnms <- getPnmsSorted
  convert pnms
  traverse_ rm pnms

convertNPictures :: Int -> MyShell ()
convertNPictures i = do
  pnmsSorted <- getPnmsSorted
  let everyN pnms
        | length pnms >= i = (take i pnms :) <$> everyN (drop i pnms)
        | null pnms = pure []
        | otherwise = throwM (PnmsNotDevidableByError i)
  everyN pnmsSorted >>= traverse_ convert
  traverse_ rm pnmsSorted

sourceToNumPicture :: ScanSource -> Int -> Int
sourceToNumPicture source =
  case source of
    Back -> id
    ADFDuplex -> (*2)

page :: ScanSource -> Int -> MyShell ()
page scanSource i = do
  scanMultible scanSource
  convertNPictures (sourceToNumPicture scanSource i)

scanMultible :: ScanSource -> MyShell ()
scanMultible scanSource = do
  i <- getMaxFile pnmNumMatch
  scanimage scanSource ["--batch-start", T.pack (show i)]
  echo "Press y or Y to add pages, any other char will finish scan"
  whenM (flip elem ['y', 'Y'] <$> liftIO getChar) $
    scanMultible scanSource

data ScanSource =
  Back
  | ADFDuplex

isExitKey :: Char -> Bool
isExitKey = (`elem` ['q', 'Q', 'X', 'x'])

scanimage :: ScanSource -> [Text] -> MyShell ()
scanimage source extraOptions = do
  device <- asks scanner
  catch
    ( sh $
        inproc
          "scanimage"
          ( ["--batch", "-d", device, "--mode", "Color", "--page-height", "0"]
              <> scanSourceOption source
              <> extraOptions
          )
          mempty
    )
    ( \(_ :: SomeException) -> do
        echo "press q,Q,X,x to quit press any other key to try again"
        liftIO getChar
          >>= ( \case
                  (isExitKey -> True) -> mainMenu
                  _ -> scanimage source extraOptions
              )
    )

scanSourceOption :: ScanSource -> [Text]
scanSourceOption scanSource =
  case scanSource of
    Back -> []
    ADFDuplex -> ["--source", "ADF Duplex"]

withPageNumbers :: ScanSource -> MyShell ()
withPageNumbers scanSource = do
  echo "Enter how many pages each document has (duplex pages count as one)"
  liftIO getLine
    >>= maybe
      (echo "Invalid number" >> mainMenu)
      (page scanSource)
      . readMaybe

mainMenu :: MyShell ()
mainMenu = do
  echo "Press s for single pages"
  echo "Press d for single duplex page"
  echo "Press x,X,q or Q to exit the script"
  let invalid = do
        echo "You have entered an invallid selection!"
        echo "Please try again!"
        echo ""
        echo "Press any key to continue..."
        void $ liftIO getChar
        mainMenu
  liftIO getChar >>= \case
    's' -> withPageNumbers Back
    'd' -> withPageNumbers ADFDuplex
    (isExitKey -> True) -> lift $ exit ExitSuccess
    _ -> invalid
  mainMenu

getMaxFile :: MonadIO m => Pattern Int -> m Int
getMaxFile fileParser =
   fold
     ((match fileParser <$>)
      . toText <$> ls ".")
     (Fold.foldMap
       (\case
           Right [i] -> Max $ i + 1
           _ -> 0 )
       getMax)

main :: IO ()
main = do
 hSetBuffering stdin NoBuffering
 t <- today
 initialCounter <- getMaxFile ("./input_" *> decimal <* text ("_" <> t <> ".pdf"))
 counter <- liftIO $ newIORef initialCounter
 mayScanner <-
   fold
     (inproc "scanimage" ["-L"] mempty
      & grep (contains "epjitsu")
      & sed (chars1 *> "epjitsu" <> (T.init <$> ends "'") <* chars1))
     Fold.head
 sh $ case mayScanner of
   Just (lineToText -> scanner) ->
     (getPnms >>= rm) <|> runReaderT mainMenu (Env{..})
   Nothing -> throwM NoScanner
 `finally`
  sh (getPnms >>= rm)
