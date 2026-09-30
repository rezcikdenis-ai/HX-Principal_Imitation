{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Types where

import Data.Vector (Vector)
import GHC.Generics (Generic)

----------Позиции и алфавит

type Position = Int --Позиция на роторе с алфавитом от 0 до 41

alphabetSize :: Int
alphabetSize = 41

letterMin, letterMax :: Position
letterMin = 0
letterMax = 25

reentryMin, reentryMax :: Position
reentryMin = 26
reentryMax = 40

isLetter :: Position -> Bool
isLetter p = p >= letterMin && p <= letterMax

isReentry :: Position -> Bool
isReentry p = p >= reentryMin && p <= reentryMax

------------Проводка и штифты

--Проводка ротора: перестановка от 0 до 40
type Wiring = Vector Position

--Штифты ротора
type Pins = Vector Bool

------------Ротор

data Rotor = Rotor {
    rotorWiring :: !Wiring,
    rotorPins :: !Pins,
    rotorPosition :: !Position
} deriving (Eq, Show, Generic)

------------Модификаторы

--Перестановка
type Modifier = Vector Position
--Управление шагом роторов
type SteppingControl = Vector Int

------------Ключи

--Внутренний ключ(долговременный)

data InternalKey = InternalKey{
    ikRotors :: ![Rotor],
    ikInputModifier :: !Modifier,
    ikReentryModifier :: !Modifier,
    ikSteppingControl :: !SteppingControl
} deriving (Eq, Show, Generic)

--Ключ сообщения(сеансовый)

data MessageKey = MessageKey{
    mkPositions :: ![Position]
} deriving (Eq, Show, Generic)

------------Состояние машины

data MachineState = MachineState{
    msRotors :: ![Rotor],
    msInputModifier :: !Modifier,
    msReentryModifier :: !Modifier,
    msSteppingControl :: !SteppingControl
} deriving (Eq, Show, Generic)

