{-# LANGUAGE DerivingStrategies #-}
module Rotor where

import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Vector(Vector, (!), (//))
import qualified Data.Vector as V

import Types
import Alphabet



--------Создание и валидация

--Проверка что ротор является перестановкой
mkWiring :: Vector Position -> Maybe Wiring
mkWiring v
    | V.length v /= alphabetSize = Nothing
    | not (all isValidPosition (V.toList v)) = Nothing
    | sort (V.toList v) /= allPositions = Nothing
    | otherwise = Just v

--Проверка что вектор штифтов имеет длину 41
mkPins :: Vector Bool -> Maybe Pins
mkPins v
    | V.length v == alphabetSize = Just v
    | otherwise = Nothing

--Создание ротора
--безопасная версия
mkRotor :: Wiring -> Pins -> Position -> Maybe Rotor
mkRotor wiring pins pos = do
    w <- mkWiring wiring
    p <- mkPins pins
    if isValidPosition pos
        then Just $ Rotor w p pos
        else Nothing

--Небезопасная версия(для, например, QuickCheck или внутри генератора случайных ключей)

unsafemkRotor :: Wiring -> Pins -> Position -> Rotor
unsafemkRotor wiring pins pos = 
    fromMaybe (error "HX63.Rotor.unsafeMkRotor: invalid rotor data")
        (mkRotor wiring pins pos)

--Проверка инвариантов созданного ротора

validateRotor :: Rotor -> Bool
validateRotor (Rotor w p pos) = 
    case mkRotor w p pos of
        Just _ -> True
        Nothing -> False

----------Позиции

--Установка позиции ротора

setPosition :: Position -> Rotor -> Maybe Rotor
setPosition p rotor
    | isValidPosition p = Just rotor {rotorPosition = p}
    | otherwise = Nothing

--Сдвиг позиции на 1 вперёд

advance :: Rotor -> Rotor
advance rotor = rotor {rotorPosition = positionOffset(rotorPosition rotor) 1}

--Сдвиг позиции на n

advanceBy :: Int -> Rotor -> Rotor
advanceBy n rotor = rotor {rotorPosition = positionOffset(rotorPosition rotor) n}

-----------Штифты

--Проверка является ли штифт активным
isPinActive :: Rotor -> Bool
isPinActive rotor = pinAt (rotorPosition rotor) rotor

--Состояние штифта по индексу от 0 до 40
pinAt :: Position -> Rotor -> Bool
pinAt p rotor
    | isValidPosition p = rotorPins rotor ! p
    | otherwise = False

-----------Прохождение сигнала

--Прохождение с учётом текущего положения ротора

applyWiringAt :: Wiring -> Position -> Position -> Position
applyWiringAt wiring pos input = 
    let rel = positionOffset input (negate pos)
        mapped = wiring ! rel
    in positionOffset mapped pos

--Проходка сигнала вперёд

forward :: Rotor -> Position -> Position
forward rotor input = applyWiringAt (rotorWiring rotor) (rotorPosition rotor) input

--Проводка назад(нужна будет для re-entry)
backward :: Rotor -> Position -> Position
backward rotor input = applyWiringAt (inverseWiring (rotorWiring rotor)) (rotorPosition rotor) input

--Обратная перестановка
inverseWiring :: Wiring -> Wiring
inverseWiring wiring =
  let pairs = [ (wiring ! i, i) | i <- [0 .. alphabetSize - 1] ]
      inv   = V.replicate alphabetSize 0 // pairs
  in inv


