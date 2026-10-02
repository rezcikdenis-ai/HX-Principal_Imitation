{-# LANGUAGE DerivingStrategies #-}
module Stepping where
import qualified Data.Vector as V
import Data.Vector(Vector, (!))
import Types
import Rotor

numRotors = 9

-------Создание и валидация

-- Проверка что control длины 9 и все значения индексы от 0 до 8
isValidSteppingControl :: SteppingControl -> Bool
isValidSteppingControl c = 
    V.length c == numRotors
    && V.all (\j -> j >= 0&& j < numRotors) c


--Безопасное создание
mkSteppingControl :: [Int] -> Maybe SteppingControl
mkSteppingControl xs = 
    let v = V.fromList xs
    in if isValidSteppingControl v then Just v else Nothing

--Небезопасное
unsafeMkSteppingControl :: [Int] -> SteppingControl
unsafeMkSteppingControl xs = 
    case mkSteppingControl xs of
        Just c -> c
        Nothing -> error "HX63.Stepping.unsafeMkSteppingControl: invalid control vector"

successiveControl :: SteppingControl
successiveControl =
    V.fromList $ 0 : [0 .. numRotors - 2]

--каждый ротор контролируется собственным штифтом
identityControl :: SteppingControl
identityControl = V.fromList [0 .. numRotors - 1]

defaultControl :: SteppingControl
defaultControl = successiveControl

-------Логика шага

--список bool длины = числу роторов
whichStep :: [Rotor] -> SteppingControl -> [Bool]
whichStep rotors control =
    let n = length rotors
        ctrl i
            | isValidSteppingControl control
            , i < V.length control
            , let j = control ! i
            , j < n
            = j
            | otherwise = max 0 (i - 1)

        pinActive j
            | j >= 0 && j < n = isPinActive (rotors !! j)
            | otherwise       = False

    in [(i == 0) || pinActive (ctrl i) | i <- [0 .. n - 1]]

--Роторы, для которых whichStep трушный, продвигаются
step :: [Rotor] -> SteppingControl -> [Rotor]
step rotors control = 
    let flags = whichStep rotors control
    in zipWith 
        (\r should -> if should then advance r else r) rotors flags

--Шаг над полным состоянием машины
stepMachine :: MachineState -> MachineState
stepMachine ms = ms {msRotors = step (msRotors ms) (msSteppingControl ms)}