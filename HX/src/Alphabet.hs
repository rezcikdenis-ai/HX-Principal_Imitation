{-# LANGUAGE DerivingStrategies #-}
module Alphabet where

import Data.Char (isAsciiUpper, isAsciiLower, toUpper, ord, chr)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Types

----------Предикаты
isValidPosition :: Position -> Bool
isValidPosition p = p >= 0 && p <= 40

isValidChar :: Char -> Bool
isValidChar с = isAsciiUpper с || isAsciiLower с

----------Преобразования

charToPosition :: Char -> Maybe Position
charToPosition c 
    | isAsciiUpper c = Just (ord c - ord 'A')
    | isAsciiLower c = Just (ord c - ord 'a')
    | otherwise = Nothing

positionToChar :: Position -> Maybe Char
positionToChar p 
    | isLetter p = Just (chr (ord 'A' + p))
    | otherwise = Nothing

normalizeText = mapMaybe normalizeChar

normalizeChar :: Char -> Maybe Char
normalizeChar c
    | isValidChar c = Just (toUpper c)
    | otherwise = Nothing

----------Готовые наборы позиций

allPositions = [0..alphabetSize - 1]

allLetters = [letterMin..letterMax]

allReentry = [reentryMin..reentryMax]

--Вектор буквенных позиций

letterVector :: Vector Position
letterVector = V.fromList allLetters

reentryVector :: Vector Position
reentryVector = V.fromList allReentry

alphabetVector :: Vector Position
alphabetVector = V.fromList allPositions


---------Утилиты

--Сдвиг позиции на заданное смещение по модулю

positionOffset :: Position -> Int -> Position
positionOffset p delta = 
    let raw = p + delta
    in raw `mod` alphabetSize

--Приведение произвольного числа к числу допустимой позиции

clampPosition :: Int -> Position
clampPosition n = n `mod` alphabetSize