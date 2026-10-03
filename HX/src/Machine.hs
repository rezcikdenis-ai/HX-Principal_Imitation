{-# LANGUAGE DerivingStrategies #-}

module Machine where

import Types
import Alphabet
import Rotor
import SignalPath
import Stepping
import Data.Maybe (mapMaybe)

--Сборка этого франкенштейна из готовых частей
mkMachineState :: [Rotor] -> Modifier -> Modifier -> SteppingControl -> MachineState
mkMachineState rotors inputMod reentryMod control = 
    MachineState {
        msRotors = rotors,
        msInputModifier = inputMod,
        msReentryModifier = reentryMod,
        msSteppingControl = control
    }

--Начальное состояние из внутреннего и внешнего ключа
fromIntegralAndMessage :: InternalKey -> MessageKey -> MachineState
fromIntegralAndMessage ik mk = 
    let rotors0 = ikRotors ik
        positions = mkPositions mk
        rotors = zipWith (\r pos -> maybe r id (setPosition pos r)) rotors0 (positions ++ repeat 0)
    in MachineState
        {msRotors = rotors,
        msInputModifier = ikInputModifier ik,
        msReentryModifier = ikReentryModifier ik,
        msSteppingControl = ikSteppingControl ik
        }

------Один символ

--Шифровка одной буквенной позиции
encryptChar :: MachineState -> Position -> (Position, MachineState)
encryptChar ms input = 
    let sub = buildSubstitution ms
        output = if isLetter input
            then applySubstitution sub input
            else input
        ms' = stepMachine ms
    in (output, ms')

--Та же подставнока, но в конце ещё обратная
decryptChar :: MachineState -> Position -> (Position, MachineState)
decryptChar ms input = 
    let sub = buildSubstitution ms
        inv = inverseSubstitution sub
        output = if isLetter input
            then applySubstitution inv input 
            else input
        ms' = stepMachine ms
    in (output, ms')


------Один бувенный символ

--Шифровка одного символа
encryptLetter :: MachineState -> Char -> Maybe (Char, MachineState)
encryptLetter ms c = do
    p <- charToPosition c
    let (p', ms') = encryptChar ms p
    c' <- positionToChar p'
    pure (c', ms')

--Дешифровка одного символа
decryptLetter :: MachineState -> Char -> Maybe (Char, MachineState)
decryptLetter ms c = do
    p <- charToPosition c
    let (p', ms') = decryptChar ms p
    c' <- positionToChar p'
    pure (c', ms')


------Тексты

--Зашифровать список буквенных позиций
encrypt :: MachineState -> [Position] -> ([Position], MachineState)
encrypt ms inputs = go ms inputs []
    where
        go st [] acc = (reverse acc, st)
        go st (x:xs) acc =
            let (y, st') = encryptChar st x
            in go st' xs (y : acc)

--Расшировать список буквенных позиций
decrypt :: MachineState -> [Position] -> ([Position], MachineState)
decrypt ms inputs = go ms inputs []
    where
        go st [] acc = (reverse acc, st)
        go st (x:xs) acc =
            let (y, st') = decryptChar st x
            in go st' xs (y : acc)

--Заширофать текст
encryptText :: MachineState -> String -> (String, MachineState)
encryptText ms text =
    let letters   = normalizeText text
        positions = mapMaybe charToPosition letters
        (outPos, ms') = encrypt ms positions
        outText   = mapMaybe positionToChar outPos
    in (outText, ms')

--Расшифровать текст
decryptText :: MachineState -> String -> (String, MachineState)
decryptText ms text =
    let letters   = normalizeText text
        positions = mapMaybe charToPosition letters
        (outPos, ms') = decrypt ms positions
        outText   = mapMaybe positionToChar outPos
    in (outText, ms')