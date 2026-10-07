{-#LANGUAGE OverloadedStrings#-}

module Main (main) where
import Test.QuickCheck
import Test.Hspec

import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import System.Random (mkStdGen)

import Types
import Alphabet
import Rotor
import Stepping
import Modifier
import SignalPath
import Machine
import Key
import Config

------Вспомогательные значения
idWiring :: Wiring
idWiring = V.fromList [0 .. 40]

noPins :: Pins
noPins = V.replicate 41 False

--машина на нулевых позициях
identityMachine :: MachineState
identityMachine = fromJust (setupMachine identityInternalKey zeroMessageKey)

------Alphabet
alphabetSpec :: Spec
alphabetSpec = describe "Alphabet" $ do
    it "alphabetSize == 41" $
        alphabetSize `shouldBe` 41
    
    it "charToPosition A..Z" $
        map charToPosition ['A' .. 'Z']
        `shouldBe` map Just [0 .. 25]
    
    it "charToPosition is case-insensitive" $
        charToPosition 'a' `shouldBe` Just 0
    
    it "charToPosition rejects non-letters" $
        charToPosition '!' `shouldBe` Nothing
    
    it "positionToChar 0..25" $
        map positionToChar [0 .. 25]
        `shouldBe` map Just ['A' .. 'Z']
    
    it "positionToChar rejects re-entry" $
        positionToChar 26 `shouldBe` Nothing
    
    it "roundtrip Char -> Position -> Char for A..Z" $
        property $ \c ->
        c `elem` ['A' .. 'Z'] ==>
            (charToPosition c >>= positionToChar) == Just c
    
    it "normalizeText keeps only letters, uppercases" $
        normalizeText "Hi, 123 XYZ!" `shouldBe` "HIXYZ"
    
    it "positionOffset wraps mod 41" $ do
        positionOffset 40 1 `shouldBe` 0
        positionOffset 0 (-1) `shouldBe` 40

------Rotor
rotorSpec :: Spec
rotorSpec = describe "Rotor" $ do
    let r0 = unsafeMkRotor idWiring noPins 0
        swap01 = idWiring V.// [(0, 1), (1, 0)]
        rSwap = unsafeMkRotor swap01 noPins 0
    
    it "mkRotor accepts identity wiring" $
        isJust (mkRotor idWiring noPins 0) `shouldBe` True
    
    it "mkRotor rejects short wiring" $
        isJust (mkWiring (V.fromList [0 .. 10])) `shouldBe` False
    
    it "forward on identity is identity" $
        map (forward r0) [0 .. 40] `shouldBe` [0 .. 40]
    
    it "backward is inverse of forward" $
        property $ \(NonNegative n) ->
        let i = n `mod` 41
        in backward rSwap (forward rSwap i) == i
    
    it "advance increases position mod 41" $ do
        rotorPosition (advance r0) `shouldBe` 1
        rotorPosition (advance (unsafeMkRotor idWiring noPins 40)) `shouldBe` 0
    
    it "isPinActive reads pin at current position" $ do
        let pins = V.fromList (True : replicate 40 False)
            r = unsafeMkRotor idWiring pins 0
        isPinActive r `shouldBe` True
        isPinActive (unsafeMkRotor idWiring pins 1) `shouldBe` False

------Modifier
modifierSpec :: Spec
modifierSpec = describe "Modifier" $ do
    it "identity is valid input and reentry" $ do
        isValidInputModifier identityModifier `shouldBe` True
        isValidReentryModifier identityModifier `shouldBe` True
    
    it "applyModifierInv restores after applyModifier" $ do
        let letterSwap = V.fromList (1 : 0 : [2 .. 25])
            m = fromJust (mkInputModifier letterSwap)
        map (\i -> applyModifierInv m (applyModifier m i)) [0 .. 40]
        `shouldBe` [0 .. 40]
    
    it "input modifier leaves re-entry lines fixed" $ do
        let letterSwap = V.fromList (1 : 0 : [2 .. 25])
            m = fromJust (mkInputModifier letterSwap)
        map (applyModifier m) [26 .. 40] `shouldBe` [26 .. 40]

-------Stepping
steppingSpec :: Spec
steppingSpec = describe "Stepping" $ do
    it "successiveControl is valid" $
        isValidSteppingControl successiveControl `shouldBe` True
    
    it "numRotors == 9" $
        numRotors `shouldBe` 9
    
    it "rotor 0 always steps" $ do
        let rotors = replicate 9 (unsafeMkRotor idWiring noPins 0)
            flags = whichStep rotors successiveControl
        head flags `shouldBe` True
    
    it "with inactive pins only rotor 0 steps" $ do
        let rotors = replicate 9 (unsafeMkRotor idWiring noPins 0)
            flags = whichStep rotors successiveControl
        flags `shouldBe` (True : replicate 8 False)
    
    it "active pin on rotor 0 causes rotor 1 to step (successive)" $ do
        let pin0 = V.fromList (True : replicate 40 False)
            rotors =
                unsafeMkRotor idWiring pin0 0
                    : replicate 8 (unsafeMkRotor idWiring noPins 0)
            flags = whichStep rotors successiveControl
        (flags !! 0, flags !! 1) `shouldBe` (True, True)

-------SignalPath/Machine
machineSpec :: Spec
machineSpec = describe "Machine / SignalPath" $ do
    it "identity key gives letter permutation" $
        validateSubstitution identityMachine `shouldBe` True
    
    it "encryptChar then decryptChar recovers letter (single step)" $ do
        let (c1, _) = encryptChar identityMachine 0
            (p1, _) = decryptChar identityMachine c1
        p1 `shouldBe` 0
    
    it "encryptText . decryptText == id (letters only)" $ do
        let plain = "HELLOWORLD"
            (cipher, _) = encryptText identityMachine plain
            (recovered, _) = decryptText identityMachine cipher
        recovered `shouldBe` plain
    
    it "encryptText/decryptText roundtrip for generated key" $ do
        let g = mkStdGen 42
            (ik, mk, _) = generateKeyPair g
            ms = fromJust (setupMachine ik mk)
            plain = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            (cipher, _) = encryptText ms plain
            (recovered, _) = decryptText ms cipher
        recovered `shouldBe` plain
    
    it "ciphertext differs from plaintext for non-trivial modifier key" $ do
        let g = mkStdGen 99
            (ik, mk, _) = generateKeyPair g
            ms = fromJust (setupMachine ik mk)
            plain = "HELLOWORLD"
            (cipher, _) = encryptText ms plain
        length cipher `shouldBe` length plain

-------Key
keySpec :: Spec
keySpec = describe "Key" $ do
    it "identityInternalKey is valid" $
        validateInternalKey identityInternalKey `shouldBe` True
    
    it "zeroMessageKey is valid" $
        validateMessageKey zeroMessageKey `shouldBe` True
    
    it "identity pair passes validateKeyPair" $
        validateKeyPair identityInternalKey zeroMessageKey `shouldBe` True
    
    it "generated key pair is valid" $ do
        let g = mkStdGen 123
            (ik, mk, _) = generateKeyPair g
        validateKeyPair ik mk `shouldBe` True
    
    it "setupMachine returns Just for valid pair" $
        isJust (setupMachine identityInternalKey zeroMessageKey) `shouldBe` True

------Config(json)
configSpec :: Spec
configSpec = describe "Config JSON" $ do
    it "InternalKey encode/decode roundtrip" $ do
        let ik = identityInternalKey
            bs = encodeInternalKey ik
        decodeInternalKey bs `shouldBe` Right ik
    
    it "MessageKey encode/decode roundtrip" $ do
        let mk = zeroMessageKey
            bs = encodeMessageKey mk
        decodeMessageKey bs `shouldBe` Right mk
    
    it "generated InternalKey survives JSON" $ do
        let g = mkStdGen 7
            (ik, _, _) = generateKeyPair g
            bs = encodeInternalKey ik
        case decodeInternalKey bs of
            Left err -> expectationFailure (displayConfigError err)
            Right ik' ->
                validateInternalKey ik' `shouldBe` True
    
    it "generated MessageKey survives JSON" $ do
        let g = mkStdGen 8
            (_, mk, _) = generateKeyPair g
            bs = encodeMessageKey mk
        decodeMessageKey bs `shouldBe` Right mk


main :: IO ()
main = hspec $ do
    alphabetSpec
    rotorSpec
    modifierSpec
    steppingSpec
    machineSpec
    keySpec
    configSpec