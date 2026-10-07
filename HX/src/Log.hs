{-# LANGUAGE DerivingStrategies #-}

module Log where
import Control.Exception (try, SomeException)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.IO (IOMode(..), hPutStrLn, withFile, stderr)

--Имя файла лога по умолчанию
logFileDefault :: FilePath
logFileDefault = "hx63.log"

--Запись соо в файл лога
logAction :: String -> IO ()
logAction = logActionTo logFileDefault

--Запись соо в указанный файл
logActionTo :: FilePath -> String -> IO ()
logActionTo path msg = do
  now <- getCurrentTime
  let stamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      line  = stamp ++ " | " ++ msg
  result <- try (withFile path AppendMode $ \h -> hPutStrLn h line) :: IO (Either SomeException ())
  case result of
    Left err -> hPutStrLn stderr $ "Не удалось записать лог: " ++ show err
    Right () -> pure ()