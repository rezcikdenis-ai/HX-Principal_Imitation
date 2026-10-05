{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO.Error (tryIOError)


import Types
import Modifier
import Rotor
import Stepping
import Key

-------Ошибки

data ConfigError
  = FileNotFound FilePath
  | FileIOError FilePath String
  | ParseError String
  | ValidationError String
  deriving stock (Eq, Show)

--Вывод ошибок
displayConfigError :: ConfigError -> String
displayConfigError (FileNotFound path) =
    "Файл не найден: " ++ path
displayConfigError (FileIOError path msg) =
    "Ошибка ввода-вывода (" ++ path ++ "): " ++ msg
displayConfigError (ParseError msg) =
    "Ошибка разбора JSON: " ++ msg
displayConfigError (ValidationError msg) =
    "Ошибка валидации: " ++ msg

--json представление одного ротора.
data RotorDTO = RotorDTO
    {wiring :: [Int],
    pins :: [Bool],
    position :: Int
    } deriving stock (Eq, Show, Generic)

instance ToJSON RotorDTO where
    toJSON = genericToJSON defaultOptions

instance FromJSON RotorDTO where
    parseJSON = genericParseJSON defaultOptions

--json представление внутреннего ключа.
data InternalKeyDTO = InternalKeyDTO
    {rotors :: [RotorDTO],
    inputModifier :: [Int],
    reentryModifier :: [Int],
    steppingControl :: [Int]
    } deriving stock (Eq, Show, Generic)

instance ToJSON InternalKeyDTO where
    toJSON = genericToJSON defaultOptions

instance FromJSON InternalKeyDTO where
    parseJSON = genericParseJSON defaultOptions

--json представление ключа сообщения.
data MessageKeyDTO = MessageKeyDTO
    {positions :: [Int]
    } deriving stock (Eq, Show, Generic)

instance ToJSON MessageKeyDTO where
    toJSON = genericToJSON defaultOptions

instance FromJSON MessageKeyDTO where
    parseJSON = genericParseJSON defaultOptions


--Rotor в DTO.
rotorToDTO :: Rotor -> RotorDTO
rotorToDTO r = RotorDTO
    {wiring = V.toList (rotorWiring r),
    pins = V.toList (rotorPins r),
    position = rotorPosition r
    }
    
--DTO в Rotor (с проверкой инвариантов).
rotorFromDTO :: RotorDTO -> Either ConfigError Rotor
rotorFromDTO dto =
    case mkRotor (V.fromList (wiring dto)) (V.fromList (pins dto)) (position dto) of
    Just r  -> Right r
    Nothing -> Left $ ValidationError $
        "Некорректный ротор (pos=" ++ show (position dto)
        ++ ", wiring len=" ++ show (length (wiring dto))
        ++ ", pins len=" ++ show (length (pins dto)) ++ ")"

--InternalKey в DTO.
internalToDTO :: InternalKey -> InternalKeyDTO
internalToDTO ik = InternalKeyDTO
    {rotors = map rotorToDTO (ikRotors ik),
    inputModifier = V.toList (ikInputModifier ik),
    reentryModifier = V.toList (ikReentryModifier ik),
    steppingControl = V.toList (ikSteppingControl ik)
    }

--DTO в InternalKey с валидацией
internalFromDTO :: InternalKeyDTO -> Either ConfigError InternalKey
internalFromDTO dto = do
    rs <- mapM rotorFromDTO (rotors dto)
    inMod <- case mkFullModifier (V.fromList (inputModifier dto)) of
        Just m -> Right m
        Nothing -> Left $ ValidationError "inputModifier: не перестановка 0..40"
    reMod <- case mkFullModifier (V.fromList (reentryModifier dto)) of
        Just m -> Right m
        Nothing -> Left $ ValidationError "reentryModifier: не перестановка 0..40"
    ctrl <- case mkSteppingControl (steppingControl dto) of
        Just c -> Right c
        Nothing -> Left $ ValidationError "steppingControl: некорректные значения"
    let ik = InternalKey
            {ikRotors = rs,
            ikInputModifier = inMod,
            ikReentryModifier = reMod,
            ikSteppingControl = ctrl
            }
    if validateInternalKey ik
        then Right ik
        else Left $ ValidationError "InternalKey не прошёл validateInternalKey"

--MessageKey в DTO.
messageToDTO :: MessageKey -> MessageKeyDTO
messageToDTO mk = MessageKeyDTO {positions = mkPositions mk}

--DTO в MessageKey с валидацией
messageFromDTO :: MessageKeyDTO -> Either ConfigError MessageKey
messageFromDTO dto =
    let mk = MessageKey (positions dto)
    in if validateMessageKey mk
        then Right mk
        else Left $ ValidationError $
            "MessageKey: нужно " ++ show numRotors ++ " позиций 0..40, получено "
            ++ show (positions dto)

------Кодировка и декодировка

--Внутренний ключ в json
encodeInternalKey :: InternalKey -> BL.ByteString
encodeInternalKey = encode . internalToDTO

--json во внутренний ключ.
decodeInternalKey :: BL.ByteString -> Either ConfigError InternalKey
decodeInternalKey bs =
    case eitherDecode bs of
        Left err -> Left $ ParseError err
        Right dto -> internalFromDTO dto

--Ключ сообщения в json
encodeMessageKey :: MessageKey -> BL.ByteString
encodeMessageKey = encode . messageToDTO

--json в ключ сообщения.
decodeMessageKey :: BL.ByteString -> Either ConfigError MessageKey
decodeMessageKey bs =
    case eitherDecode bs of
        Left err -> Left $ ParseError err
        Right dto -> messageFromDTO dto

-------Файлы ключей

--Сохранить внутренний ключ в json файл.
saveInternalKey :: FilePath -> InternalKey -> IO (Either ConfigError ())
saveInternalKey path ik = writeLBS path (encodeInternalKey ik)

--Загрузить внутренний ключ из json файла.
loadInternalKey :: FilePath -> IO (Either ConfigError InternalKey)
loadInternalKey path = do
  ebs <- readLBS path
  pure $ ebs >>= decodeInternalKey

--Сохранить ключ сообщения в json файл.
saveMessageKey :: FilePath -> MessageKey -> IO (Either ConfigError ())
saveMessageKey path mk = writeLBS path (encodeMessageKey mk)

--Загрузить ключ сообщения из json файла.
loadMessageKey :: FilePath -> IO (Either ConfigError MessageKey)
loadMessageKey path = do
  ebs <- readLBS path
  pure $ ebs >>= decodeMessageKey

-------Текстовые файлы

-- прочитать текстовый файл (открытый/шифротекст).
readPlaintext :: FilePath -> IO (Either ConfigError String)
readPlaintext path = do
    exists <- doesFileExist path
    if not exists
        then pure $ Left (FileNotFound path)
        else do
        result <- tryIOError (readFile path)
        pure $ case result of
            Left err -> Left $ FileIOError path (show err)
            Right s -> Right s

--Записать шифротекст в файл.
writeCiphertext :: FilePath -> String -> IO (Either ConfigError ())
writeCiphertext = writeTextFile

--Записать строку в файл.
writeTextFile :: FilePath -> String -> IO (Either ConfigError ())
writeTextFile path content = do
    result <- tryIOError (writeFile path content)
    pure $ case result of
        Left err -> Left $ FileIOError path (show err)
        Right () -> Right ()

-------Немного IO-хелперов

--Прочитать файл как lazy ByteString
readLBS :: FilePath -> IO (Either ConfigError BL.ByteString)
readLBS path = do
    exists <- doesFileExist path
    if not exists
        then pure $ Left (FileNotFound path)
        else do
            result <- tryIOError (BL.readFile path)
            pure $ case result of
                Left err -> Left $ FileIOError path (show err)
                Right bs -> Right bs

--Записать lazy ByteString в файл
writeLBS :: FilePath -> BL.ByteString -> IO (Either ConfigError ())
writeLBS path bs = do
    result <- tryIOError (BL.writeFile path bs)
    pure $ case result of
        Left err -> Left $ FileIOError path (show err)
        Right () -> Right ()