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

--Случайный входной модификатор
generateInputModifier :: RandomGen g => g -> (Modifier, g)
generateInputModifier g0 =
    let (perm, g1) = shuffle allLetters g0
        m = case mkInputModifier (V.fromList perm) of
            Just x  -> x
            Nothing -> identityModifier
    in (m, g1)

--Случайный reentry модификатор
generateReentryModifier :: RandomGen g => g -> (Modifier, g)
generateReentryModifier g0 =
    let (perm, g1) = shuffle allReentry g0
        m = case mkReentryModifier (V.fromList perm) of
            Just x  -> x
            Nothing -> identityModifier
    in (m, g1)

--Тожд.проводка и штифты
generateRotor :: RandomGen g => g -> (Rotor, g)
generateRotor g0 =
    let (pins, g1) = generatePins g0
        r = case mkRotor identityWiring pins 0 of
            Just x  -> x
            Nothing -> unsafeMkRotor identityWiring emptyPins 0
    in (r, g1)

--------Случайный внутренний ключ
generateInternalKey :: RandomGen g => g -> (InternalKey, g)
generateInternalKey g0 =
  let (rotors, g1) = generateRotors numRotors g0
      (inMod,  g2) = generateInputModifier g1
      (reMod,  g3) = generateReentryModifier g2
  in ( InternalKey
         { ikRotors          = rotors
         , ikInputModifier   = inMod
         , ikReentryModifier = reMod
         , ikSteppingControl = successiveControl
         }
     , g3
     )
  where
    generateRotors 0 g = ([], g)
    generateRotors n g =
      let (r, g')   = generateRotor g
          (rs, g'') = generateRotors (n - 1) g'
      in (r : rs, g'')

--Случайная пара ключей
generateKeyPair :: RandomGen g => g -> (InternalKey, MessageKey, g)
generateKeyPair g0 =
    let (ik, g1) = generateInternalKey g0
        (mk, g2) = generateMessageKey g1
    in (ik, mk, g2)