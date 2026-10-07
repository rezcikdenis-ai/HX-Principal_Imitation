module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Random (newStdGen)

import Types
import Key
import Config
import Machine
import Log

--Точка входа
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usage >> exitFailure
        ("help":_) -> usage >> exitSuccess
        ("-h":_) -> usage >> exitSuccess
        ("--help":_) -> usage >> exitSuccess
        (cmd:rest) -> do 
            logAction $ "команда: " ++ cmd ++ " " ++ unwords rest
            runCommand cmd rest

usage :: IO ()
usage = putStrLn $ unlines
    ["HX-63 simulator",
     "",
     "Использование:",
     "  hx63-sim-exe gen-key  --internal FILE --message FILE",
     "  hx63-sim-exe encrypt  --internal FILE --message FILE -i IN -o OUT",
     "  hx63-sim-exe decrypt  --internal FILE --message FILE -i IN -o OUT",
     "",
     "Опции:",
     "  --internal FILE   JSON внутреннего ключа",
     "  --message FILE    JSON ключа сообщения",
     "  -i, --input FILE  входной текст",
     "  -o, --output FILE выходной текст",
     "  -h, --help        эта справка"
    ]

-------Разбор опций

--Достать значение опции
getOpt :: String -> [String] -> Maybe String
getOpt _ [] = Nothing
getOpt name (a:b:rest)
    | a == name = Just b
    | otherwise = getOpt name (b:rest)
getOpt name [_] = Nothing

--Достать для длинных имён 
getOptAny :: [String] -> [String] -> Maybe String
getOptAny names args = foldr (\n acc -> maybe (getOpt n args) Just acc) Nothing names

requireOpt :: String -> [String] -> [String] -> IO String
requireOpt label names args =
    case getOptAny names args of
        Just v  -> pure v
        Nothing -> do
            let msg = "Не указана обязательная опция: " ++ label
            hPutStrLn stderr msg
            logAction $ "ошибка: " ++ msg
            usage
            exitFailure


--------Команды

runCommand :: String -> [String] -> IO ()
runCommand "gen-key" args = cmdGenKey args
runCommand "encrypt" args = cmdCrypt "encrypt" encryptText args
runCommand "decrypt" args = cmdCrypt "decrypt" decryptText args
runCommand other _ = do
    let msg = "Неизвестная команда: " ++ other
    hPutStrLn stderr msg
    logAction $ "ошибка: " ++ msg
    usage
    exitFailure


--Генерация пары ключей и запись в json
cmdGenKey :: [String] -> IO ()
cmdGenKey args = do
    internalPath <- requireOpt "--internal" ["--internal"] args
    messagePath  <- requireOpt "--message"  ["--message"] args
    g <- newStdGen
    let (ik, mk, _) = generateKeyPair g
    saveInternalKey internalPath ik >>= report
    saveMessageKey messagePath mk >>= report
    logAction $ "gen-key: ключи записаны"
    putStrLn $ "Ключи записаны:\n  internal: " ++ internalPath ++ "\n  message:  " ++ messagePath

--Кодирование или декодирование текстового файла
cmdCrypt :: String -> (MachineState -> String -> (String, MachineState)) -> [String] -> IO ()
cmdCrypt mode transform args = do
    internalPath <- requireOpt "--internal" ["--internal"] args
    messagePath  <- requireOpt "--message"  ["--message"] args
    inputPath <- requireOpt "-i/--input" ["-i", "--input"] args
    outputPath <- requireOpt "-o/--output" ["-o", "--output"] args
    logAction $ mode ++ ": internal=" ++ internalPath
               ++ " message=" ++ messagePath
               ++ " input=" ++ inputPath
               ++ " output=" ++ outputPath
    ik <- loadInternalKey internalPath >>= unwrap
    mk <- loadMessageKey messagePath >>= unwrap
    ms <- case setupMachine ik mk of
        Just st -> pure st
        Nothing -> do
            let msg = "Пара ключей невалидна (структура или подстановка букв)."
            hPutStrLn stderr msg
            logAction $ "ошибка: " ++ msg
            exitFailure

    text <- readPlaintext inputPath >>= unwrap
    let (result, _) = transform ms text
    writeCiphertext outputPath result >>= report
    logAction $ mode ++ ": успех, символов=" ++ show (length result)
               ++ " -> " ++ outputPath
    putStrLn $ "Готово → " ++ outputPath

-------Вспомогательные функции
unwrap :: Either ConfigError a -> IO a
unwrap (Right x) = pure x
unwrap (Left e) = do
    let msg = displayConfigError e
    hPutStrLn stderr msg
    logAction $ "ошибка: " ++ msg
    exitFailure

report :: Either ConfigError () -> IO ()
report (Right ()) = pure ()
report (Left e) = do
    let msg = displayConfigError e
    hPutStrLn stderr msg
    logAction $ "ошибка: " ++ msg
    exitFailure

