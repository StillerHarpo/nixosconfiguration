{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Main where

import qualified Control.Foldl as Fold
import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Foldable (traverse_)
import Data.IORef
import Data.Semigroup (Max (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format
import System.Directory.Extra (doesFileExist)
import System.IO (BufferMode (..), hSetBuffering, stdin)
import System.IO.Extra (withTempDir, withTempFile)
import Text.Read (readMaybe)
import Turtle hiding (stdin)
import Prelude hiding (FilePath)

data Env = Env
  { counter :: IORef Int,
    scanner :: Text,
    tesseractConf :: Text
  }

data CustomError
  = FileExistError FilePath
  | DirNotExistsError FilePath
  | EncodingError
  | NoScanner

instance Show CustomError where
  show = \case
    FileExistError filePath -> T.unpack $ format ("Error file " % fp % " exist") filePath
    DirNotExistsError filePath -> T.unpack $ format ("Directory " % fp % " does not exist") filePath
    EncodingError -> "encoding failed"
    NoScanner -> "Couldn't find scanner"

instance Exception CustomError

type MyShell a = ReaderT Env Shell a

_PDFS_DIR :: FilePath
_PDFS_DIR = "/home/florian/Dokumente/scans"

getCounter :: MyShell Int
getCounter = asks counter >>= liftIO . readIORef

incCounter :: MyShell ()
incCounter = asks counter >>= liftIO . flip modifyIORef' (+ 1)

getTifs :: Shell FilePath
getTifs = mfilter ((== Just "tif") . snd . splitExtension) (ls ".")

getPdfAndTxt :: MyShell (Text, Text)
getPdfAndTxt = lift getTifs >>= runTesseract

fileNumMatch :: Pattern Text -> Pattern Int
fileNumMatch = (many dot *> "out" *> decimal <*)

tifNumMatch :: Pattern Int
tifNumMatch = fileNumMatch ".tif"

pdfNumMatch :: Pattern Int
pdfNumMatch = fileNumMatch ".pdf"

delFile :: MonadIO io => FilePath -> io ()
delFile file = do
  printf ("deleting " % fp % "\n") file
  rm file

runTesseract :: FilePath -> MyShell (Text, Text)
runTesseract inputFile =
  let inputFileT = T.pack inputFile
      outputBaseT = T.pack (basename inputFile)
   in
    do
      Env {..} <- ask
      printf ("running tesseract on " % fp % "to generate pdf \n") inputFile
      procs "tesseract" [inputFileT, outputBaseT, "-l", "deu", tesseractConf] mempty
      procs "tesseract" [inputFileT, outputBaseT, "-l", "deu"] mempty
      liftIO $
        whenM
          (not <$> doesFileExist (T.unpack $ outputBaseT <> ".pdf"))
          (procs "tiff2pdf" [inputFileT] mempty)
      printf ("running textcleaner on " % fp % "\n") inputFile
      liftIO $
        withTempFile $ \((<> ".tif") -> cleanedPicture) -> do
          procs
            "textcleaner"
            ["-g", "-e", "normalize", "-f", "30", "-o", "12", "-s", "2", inputFileT, T.pack cleanedPicture]
            mempty
          printf ("running tesseract on " % fp % "to generate textfile \n") cleanedPicture
          procs "tesseract" [T.pack cleanedPicture, outputBaseT, "-l", "deu"] mempty
      delFile inputFile
      pure (outputBaseT <> ".pdf", outputBaseT <> ".txt")

liftFunReader :: Monad m => (m a -> m b) -> ReaderT s m a -> ReaderT s m b
liftFunReader fun r = ask >>= lift . fun . runReaderT r

getPdfsAndTxtsSorted :: MyShell [(Text, Text)]
getPdfsAndTxtsSorted = liftFunReader (sortOn (maximum . match pdfNumMatch . fst)) getPdfAndTxt

fileExistsError :: FilePath -> MyShell ()
fileExistsError file = do
  printf ("Error file" % fp % "exist" % "\n") file
  curCount <- getCounter
  printf ("counter was: " % d % "\n") curCount
  lift $ exit (ExitFailure 1)

today :: MonadIO m => m Text
today = T.pack . formatTime defaultTimeLocale "%d_%m_%Y" <$> date

getFileNamePdf :: MyShell Text
getFileNamePdf = do
  cur <- today
  curCount <- getCounter
  pure $ format ("input_" % d % "_" % s % ".pdf") curCount cur

getFileNameTxt :: MyShell Text
getFileNameTxt = do
  cur <- today
  curCount <- getCounter
  pure $ format ("input_" % d % "_" % s % ".txt") curCount cur

checkFile :: FilePath -> MyShell ()
checkFile file = whenM (testfile file) (fileExistsError file)

convert :: [(Text, Text)] -> MyShell ()
convert pdfsAndTxts = do
  let pdfs = map fst pdfsAndTxts
  let txts = map snd pdfsAndTxts
  convertPdfs pdfs
  convertTxts txts
  incCounter
  traverse_ (delFile . T.unpack) pdfs
  traverse_ (delFile . T.unpack) txts

convertPdfs :: [Text] -> MyShell ()
convertPdfs inputs = do
  fnT <- getFileNamePdf
  let fn = T.unpack fnT
  checkFile fn
  printf ("Writing " % fp % " from files " % s % "\n") fn (T.intercalate ", " inputs)
  procs "pdfunite" (inputs <> [fnT]) mempty
  checkFile (_PDFS_DIR <> fn)
  mv fn (_PDFS_DIR <> fn)

convertTxts :: [Text] -> MyShell ()
convertTxts inputs = do
  fnT <- getFileNameTxt
  let fn = T.unpack fnT
  checkFile fn
  printf ("Writing " % fp % " from files " % s % "\n") fn (T.intercalate ", " inputs)
  liftIO $ traverse (readFile . T.unpack) inputs >>= writeFile (T.unpack fnT) . mconcat
  checkFile (_PDFS_DIR <> fn)
  mv fn (_PDFS_DIR <> fn)

convertPictures :: MyShell ()
convertPictures = do
  (pdf, txt) <- getPdfAndTxt
  convertPdfs [pdf]
  convertTxts [txt]
  delFile (T.unpack pdf)
  delFile (T.unpack txt)
  incCounter

convertMultiblePictures :: MyShell ()
convertMultiblePictures = getPdfsAndTxtsSorted >>= convert

convertNPictures :: Int -> MyShell ()
convertNPictures i = do
  pdfsAndTxtsSorted <- getPdfsAndTxtsSorted
  let everyN pdfs
        | length pdfs >= i = (take i pdfs :) <$> everyN (drop i pdfs)
        | null pdfs = pure []
        | otherwise = pure [pdfs]
  everyN pdfsAndTxtsSorted >>= traverse_ convert

sourceToNumPicture :: ScanSource -> Int -> Int
sourceToNumPicture source =
  case source of
    Back -> id
    ADFDuplex -> (* 2)

page :: ScanSource -> Int -> MyShell ()
page scanSource i = do
  scanMultible scanSource
  convertNPictures (sourceToNumPicture scanSource i)

scanMultible :: ScanSource -> MyShell ()
scanMultible scanSource = do
  start <- getMaxTifFile tifNumMatch
  scanimage scanSource start
  echo "Press y or Y to add pages, any other char will finish scan"
  whenM (flip elem ['y', 'Y'] <$> liftIO getChar) $
    scanMultible scanSource

data ScanSource
  = Back
  | ADFDuplex

isExitKey :: Char -> Bool
isExitKey = (`elem` ['q', 'Q', 'X', 'x'])

isExitKeyStr :: String -> Bool
isExitKeyStr = \case
  [] -> False
  (c : cs) -> null cs && isExitKey c

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
      newStart <- getMaxTifFile tifNumMatch
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

getMaxFile :: MonadIO m => FilePath -> Pattern Int -> m Int
getMaxFile filePath fileParser =
  fold
    ( match (many dot *> fileParser) . T.pack
        <$> ls filePath
    )
    ( Fold.foldMap
        ( \case
            [i] -> Max $ i + 1
            _ -> 0
        )
        getMax
    )

getMaxPdfFile :: MonadIO m => Pattern Int -> m Int
getMaxPdfFile = getMaxFile _PDFS_DIR

getMaxTifFile :: MonadIO m => Pattern Int -> m Int
getMaxTifFile = getMaxFile "."

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  unlessM (testdir _PDFS_DIR) (throwM $ DirNotExistsError _PDFS_DIR)
  t <- today
  initialCounter <- getMaxPdfFile ("input_" *> decimal <* text ("_" <> t <> ".pdf"))
  putStrLn $ "start count from: " <> show initialCounter
  counter <- liftIO $ newIORef initialCounter
  mayScanner <-
    fold
      ( inproc "scanimage" ["-L"] mempty
          & grep (contains "epjitsu")
          & sed (chars1 *> "epjitsu" <> (T.init <$> ends "'") <* chars1)
      )
      Fold.head
  case mayScanner of
    Just (lineToText -> scanner) ->
      withTempDir $ \dir ->
        withTempFile $ \tesseractConfStr -> do
          echo "writing tesseract config"
          writeFile tesseractConfStr "tessedit_create_pdf 1"
          cd dir
          pwd >>= printf ("current directory is now: " % fp % "\n")
          let tesseractConf = T.pack tesseractConfStr
          sh $ runReaderT mainMenu (Env {..})
    Nothing -> throwM NoScanner
