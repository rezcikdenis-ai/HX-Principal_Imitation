{-# LANGUAGE DerivingStrategies #-}
module Modifier where

import Data.List(sort)
import Data.Maybe(fromMaybe)
import Data.Vector(Vector, (!), (//))
import qualified Data.Vector as V

import Types
import Alphabet

--Проверка что вектор перестановка заданного числа позиций
isPermutationOf :: [Position] -> Vector Position -> Bool
isPermutationOf expected v = 
    V.length v == length expected
    && all isValidPosition (V.toList v)
    && sort (V.toList v) == sort expected

--Полный модификатор длины 41
buildPartialModifier :: [Position] -> Vector Position -> Maybe Modifier
buildPartialModifier domain perm
    | not (isPermutationOf domain perm) = Nothing
    | otherwise = 
        let base = alphabetVector 
            pairs = zip domain (V.toList perm)
        in Just $ base // pairs

--------Создание

--Входной модификатор
mkInputModifier :: Vector Position -> Maybe Modifier
mkInputModifier letterPerm = buildPartialModifier allLetters letterPerm 

--reentry модификатор
mkReentryModifier :: Vector Position -> Maybe Modifier
mkReentryModifier reentryPerm = buildPartialModifier allReentry reentryPerm

--модификатор всех позиций
mkFullModifier :: Vector Position -> Maybe Modifier
mkFullModifier v
    | isPermutationOf allPositions v = Just v
    | otherwise = Nothing

--Тождественный модификатор
identityModifier :: Modifier
identityModifier = alphabetVector

--Тождественный входной модификатор
identityInput :: Modifier
identityInput = identityModifier

--Тождественный reentry модификатор
identityReentry :: Modifier
identityReentry = identityModifier


-------Валидация
--Проверка входного модификатроа на корректность
isValidInputModifier :: Modifier -> Bool
isValidInputModifier m = 
    V.length m == alphabetSize
    && sort [m ! i | i <- allLetters] == allLetters
    && and [m ! i == i | i <- allReentry]

--Проверка reentry модификатора на корректность

isValidReentryModifier :: Modifier -> Bool
isValidReentryModifier m =
  V.length m == alphabetSize
    && sort [ m ! i | i <- allReentry] == allReentry
    && and [ m ! i == i | i <- allLetters]

--Проверка полного модификатора
isValidFullModifier :: Modifier -> Bool
isValidFullModifier m = isPermutationOf allPositions m

-------Применение

--применение модфиикатора к позиции
applyModifier :: Modifier -> Position -> Position
applyModifier m p 
    | isValidPosition p = m ! p
    | otherwise = p

--примернение обратного модификатора
applyModifierInv :: Modifier -> Position -> Position
applyModifierInv m = applyModifier $ inverseModifier m

--Обратный модификатор
inverseModifier :: Modifier -> Modifier
inverseModifier m = 
    let pairs = [(m ! i, i) | i <- [0..alphabetSize - 1]]
    in V.replicate alphabetSize 0 // pairs

---------Утилиты

--безопасная версия модификатора из списка позиций
modifierFromList :: [Position] -> Maybe Modifier
modifierFromList xs = mkFullModifier (V.fromList xs)

unsafeModifierFromList :: [Position] -> Modifier
unsafeModifierFromList xs = 
    fromMaybe (error "HX63.Modifier.unsafeModifierFromList: not a permutation")
    (modifierFromList xs)