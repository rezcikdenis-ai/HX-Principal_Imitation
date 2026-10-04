{-# LANGUAGE DerivingStrategies #-}

module Key where

--import Data.Vector
import qualified Data.Vector as V
import System.Random(RandomGen, randomR)

import Types
import Alphabet
import Rotor
import Modifier
import Stepping
import Machine
import SignalPath

-------Валидация

--структурная проверка внутреннего ключа
validateInternalKey :: InternalKey -> Bool
validateInternalKey ik =
    length (ikRotors ik) == numRotors
        && all validateRotor (ikRotors ik)
        && isValidInputModifier (ikInputModifier ik)
        && isValidReentryModifier (ikReentryModifier ik)
        && isValidSteppingControl (ikSteppingControl ik)

--проверка ключа сообщения
validateMessageKey :: MessageKey -> Bool
validateMessageKey mk = 
    length (mkPositions mk) == numRotors
    && all isValidPosition (mkPositions mk)

validateKeyPair :: InternalKey -> MessageKey -> Bool
validateKeyPair ik mk = 
    validateInternalKey ik
    && validateMessageKey mk
    && validateSubstitution (fromIntegralAndMessage ik mk)

-------Сборка

--Начальное состояние машины из пары ключей
setupMachine :: InternalKey -> MessageKey -> Maybe MachineState
setupMachine ik mk
    | validateKeyPair ik mk = Just (fromIntegralAndMessage ik mk)
    | otherwise = Nothing

-------Значения по умолчанию

--Тождественная проводка 0..40
identityWiring :: Wiring
identityWiring = V.fromList allPositions

--Все штифты неактивны
emptyPins :: Pins
emptyPins = V.replicate alphabetSize False

--Внутренний ключ: 9 роторов с id-проводкой, пустые штифты, тожд.модификаторы
identityInternalKey :: InternalKey
identityInternalKey = 
    InternalKey{
        ikRotors = replicate numRotors (unsafeMkRotor identityWiring emptyPins 0),
        ikInputModifier = identityModifier,
        ikReentryModifier = identityModifier,
        ikSteppingControl = successiveControl
    }

--Все нач. позиции 0
zeroMessageKey :: MessageKey
zeroMessageKey = MessageKey (replicate numRotors 0)

------Случайная генерация
shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle xs g0 = go (length xs - 1) xs g0
    where
        go i ys g
            | i <= 0 = (ys, g)
            | otherwise = 
                let (j, g') = randomR (0, i) g
                    ys' = swap i j ys
                in go (i - 1) ys' g'
        swap i j ys = 
            let yi = ys !! i
                yj = ys !! j
            in [if k == i then yj else if k == j then yi else y | (k, y) <- zip [0 ..] ys]

--Случайный вектор штифтов
generatePins :: RandomGen g => g -> (Pins, g)
generatePins g0 =
    let go 0 g acc = (V.fromList (reverse acc), g)
        go n g acc =
            let (b, g') = randomR (False, True) g
            in go (n - 1) g' (b : acc)
    in go alphabetSize g0 []

--Случайный ключ сообщения
generateMessageKey :: RandomGen g => g -> (MessageKey, g)
generateMessageKey g0 =
    let go 0 g acc = (MessageKey (reverse acc), g)
        go n g acc =
            let (p, g') = randomR (0, alphabetSize - 1) g
            in go (n - 1) g' (p : acc)
    in go numRotors g0 []