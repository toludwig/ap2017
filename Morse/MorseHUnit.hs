{-
Example code produced for AP lecture.

Using HUnit to test the Morse module

Author: Ken Friis Larsen <kflarsen@diku.dk>
-}

import Test.HUnit
import qualified Morse
import qualified System.Random as R

test1 = TestCase $
        assertBool "Decode Sofia"
        $ "SOFIA" `elem` Morse.decode "...---..-....-"

test2 = TestCase $
        assertBool "Decode Eugenia"
        $ "EUGENIA" `elem` Morse.decode "...---..-....-"

tests = TestList [TestLabel "Decode" $ TestList [test1, test2],
                  TestLabel "Encode" $ "-.-.-." ~=? (Morse.encode "KEN"),
                  random_input]

random_string seed len = take len nonsense
  where gen = R.mkStdGen seed
        letters = ['A'..'Z']
        indexes = R.randomRs (0, length letters - 1) gen
        nonsense = map (letters !!) indexes

random_input = TestCase $
               assertBool "Random decode/encode roundtrip" $
               nonsense `elem` (Morse.decode(Morse.encode nonsense))
  where nonsense = random_string 42 4


main = runTestTT tests
