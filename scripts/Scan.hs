{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (FilePath)
import Turtle hiding (stdin)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.IORef
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Semigroup(Max(..))
import Data.Foldable(traverse_)
import Data.Time.Format
import System.IO(hSetBuffering, BufferMode(..), stdin)
import Control.Monad.Catch
import Control.Monad.Extra
import Text.Read (readMaybe)
import System.IO.Extra (withTempDir, withTempFile)

data Env = Env { counter :: IORef Int
               , scanner :: Text
               , tesseractConf :: Text }

data CustomError =
  FileExistError FilePath
  | DirNotExistsError FilePath
  | EncodingError
  | NotDevidableByError Int
  | NoScanner

instance Show CustomError where
  show = \case
     FileExistError filePath -> T.unpack $ format ("Error file "%fp%" exist") filePath
     DirNotExistsError filePath -> T.unpack $ format ("Directory "%fp%" does not exist") filePath
     EncodingError -> "encodig failed"
     NotDevidableByError n -> "internal error: number of input files should be devideable by " <> show n
     NoScanner -> "Couldn't find scanner"

instance Exception CustomError

type MyShell a = ReaderT Env Shell a

_PDFS_DIR :: FilePath
_PDFS_DIR = "/home/florian/paperlessInput/"

getCounter :: MyShell Int
getCounter = asks counter >>= liftIO . readIORef

incCounter :: MyShell ()
incCounter = asks counter >>= liftIO . flip modifyIORef' (+ 1)

getTifs :: Shell FilePath
getTifs = mfilter ((== Just "tif") . snd . splitExtension) (ls ".")

getPdfs :: MyShell Text
getPdfs = lift getTifs >>= runTesseract

fileNumMatch :: Pattern Text -> Pattern Int
fileNumMatch = ("./out" *> decimal <*)

tifNumMatch :: Pattern Int
tifNumMatch =  fileNumMatch ".tif"

pdfNumMatch :: Pattern Int
pdfNumMatch =  fileNumMatch ".pdf"

delFile :: MonadIO io => FilePath -> io ()
delFile file = do
  printf ("delting " % fp % "\n") file
  rm file

runTesseract :: FilePath -> MyShell Text
runTesseract inputFile =
  case (toText inputFile, toText (basename inputFile)) of
    (Right inputFileT, Right outputBaseT) -> do
      Env {..} <- ask
      printf ("running tesseract on " % fp % "\n") inputFile
      procs "tesseract" [inputFileT, outputBaseT, "-l", "deu", tesseractConf] mempty
      delFile inputFile
      pure (outputBaseT <> ".pdf")
    _ -> throwM EncodingError

liftFunReader :: Monad m => (m a -> m b) -> ReaderT s m a -> ReaderT s m b
liftFunReader fun r =  ask >>= lift . fun . runReaderT r

getPdfsSorted :: MyShell [Text]
getPdfsSorted = liftFunReader (sortOn (match pdfNumMatch)) getPdfs

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

convert :: [Text] -> MyShell ()
convert inputs = do
  fnT <- getFileName
  let fn = fromText fnT
  checkFile fn
  printf ("Writing "%fp%" from files "%s%"\n") fn (T.intercalate ", " inputs)
  procs "pdfunite" (inputs <> [fnT]) mempty
  mv fn (_PDFS_DIR <> fn)
  incCounter

convertPictures :: MyShell ()
convertPictures = do
  pdf <- getPdfs
  convert [pdf]
  delFile (fromText pdf)

convertMultiblePictures :: MyShell ()
convertMultiblePictures = do
  pdfs <- getPdfsSorted
  convert pdfs
  traverse_ (delFile . fromText) pdfs

convertNPictures :: Int -> MyShell ()
convertNPictures i = do
  pdfsSorted <- getPdfsSorted
  let everyN pdfs
        | length pdfs >= i = (take i pdfs :) <$> everyN (drop i pdfs)
        | null pdfs = pure []
        | otherwise = throwM (NotDevidableByError i)
  everyN pdfsSorted >>= traverse_ convert
  traverse_ (delFile . fromText) pdfsSorted

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
  start <- getMaxFile tifNumMatch
  scanimage scanSource start
  echo "Press y or Y to add pages, any other char will finish scan"
  whenM (flip elem ['y', 'Y'] <$> liftIO getChar) $
    scanMultible scanSource

data ScanSource =
  Back
  | ADFDuplex

isExitKey :: Char -> Bool
isExitKey = (`elem` ['q', 'Q', 'X', 'x'])

isExitKeyStr :: String -> Bool
isExitKeyStr = \case
  [] -> False
  (c:cs) -> null cs && isExitKey c

mainMenuKeysHint :: MonadIO io => io ()
mainMenuKeysHint = echo "press q,Q,X,x to enter main menu"

scanimage :: ScanSource -> Int -> MyShell ()
scanimage source start = do
  device <- asks scanner
  (exitCode, stdOut) <-
    procStrict
      "scanimage"
      ( ["--batch", "-d", device, "--mode", "Color", "--page-height", "0", "--format=tiff"]
          <> scanSourceOption source
          <> ["--batch-start", T.pack (show start)]
      )
      mempty
  liftIO $ T.putStrLn stdOut
  case exitCode of
    ExitSuccess -> pure ()
    (ExitFailure _) -> do
      newStart <- getMaxFile tifNumMatch
      echo "Exception occured"
      mainMenuKeysHint
      echo "Any other key will try again"
      liftIO getChar
        >>= ( \case
                (isExitKey -> True) -> mainMenu
                _ -> scanimage source newStart
            )

scanSourceOption :: ScanSource -> [Text]
scanSourceOption scanSource =
  case scanSource of
    Back -> []
    ADFDuplex -> ["--source", "ADF Duplex"]

withPageNumbers :: ScanSource -> MyShell ()
withPageNumbers scanSource = do
  echo "Enter how many pages each document has (duplex pages count as one)"
  mainMenuKeysHint
  liftIO getLine
    >>= \case
          (readMaybe -> Just n) -> page scanSource n
          (isExitKeyStr -> True) -> mainMenu
          _ -> do
             echo "Invalid number"
             withPageNumbers scanSource

mainMenu :: MyShell ()
mainMenu = do
  echo "Press s for single pages"
  echo "Press d for single duplex page"
  echo "Press x,X,q or Q to exit the script"
  let invalid = do
        echo "You have entered an invalid selection!"
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
      . toText <$> ls _PDFS_DIR)
     (Fold.foldMap
       (\case
           Right [i] -> Max $ i + 1
           _ -> 0 )
       getMax)

main :: IO ()
main = do
 hSetBuffering stdin NoBuffering
 unlessM (testdir _PDFS_DIR) (throwM $ DirNotExistsError _PDFS_DIR)
 t <- today
 initialCounter <- getMaxFile ("./input_" *> decimal <* text ("_" <> t <> ".pdf"))
 counter <- liftIO $ newIORef initialCounter
 mayScanner <-
   fold
     (inproc "scanimage" ["-L"] mempty
      & grep (contains "epjitsu")
      & sed (chars1 *> "epjitsu" <> (T.init <$> ends "'") <* chars1))
     Fold.head
 case mayScanner of
   Just (lineToText -> scanner) ->
     withTempDir $ \dir ->
       withTempFile $ \tesseractConfStr -> do
         echo "writing tesseract config"
         writeFile tesseractConfStr "tessedit_create_pdf 1"
         cd $ fromText (T.pack dir)
         pwd >>= printf ("current directory is now: "%fp%"\n")
         let tesseractConf = T.pack tesseractConfStr
         sh $ runReaderT mainMenu (Env{..})
   Nothing -> throwM NoScanner
