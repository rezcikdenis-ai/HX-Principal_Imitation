{-# LANGUAGE DerivingStrategies #-}
module SignalPath where
import Data.List(sort)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

import Types
import Alphabet
import Rotor
import Modifier

--------Путь сигнала

--Полный путь сигнала через машину в текущем состоянии
signalPath :: MachineState -> Position -> Position
signalPath ms input = 
    let p1 = applyModifier (msInputModifier ms) input
        p2 = forwardRotors (msRotors ms) p1
        p3 = applyModifier (msReentryModifier ms) p2
        p4 = backwardRotors (msRotors ms) p3
    in p4

--Прямой проход
forwardRotors :: [Rotor] -> Position -> Position
forwardRotors rotors p = foldl (\acc r -> forward r acc) p rotors

--Обратный проход от последнего ротора к 0
backwardRotors :: [Rotor] -> Position -> Position
backwardRotors rotors p = foldl (\acc r -> backward r acc) p (reverse rotors)

-------Подстановка на буквах

--Таблица подстановки для букв в текущем состоянии
buildSubstitution :: MachineState -> Vector Position
buildSubstitution ms = 
    V.generate (letterMax - letterMin + 1) $ \i -> signalPath ms i

--Обратная подстановка
inverseSubstitution :: Vector Position -> Vector Position
inverseSubstitution sub = 
    let n = V.length sub
        pairs = [(sub ! i, i) | i <- [0 .. n - 1], isLetter (sub ! i)]
        base  = V.replicate n 0
    in base // pairs

--Применение таблицы подстановки к буквенной позиции
applySubstitution :: Vector Position -> Position -> Position
applySubstitution sub p
    | isLetter p && p < V.length sub = sub ! p
    | otherwise = p

--Применение обратной таблицы
applySubstitutionInv :: Vector Position -> Position -> Position
applySubstitutionInv sub = applySubstitution (inverseSubstitution sub)

--------Проверки

--Является ли вектор перестановкой множества 0..25
isLetterPermutation :: Vector Position -> Bool
isLetterPermutation sub =
  V.length sub == 26
    && V.all isLetter sub
    && sort (V.toList sub) == allLetters

--Проверка подстановки из построенной из состояния машины
validateSubstitution :: MachineState -> Bool
validateSubstitution = isLetterPermutation . buildSubstitution