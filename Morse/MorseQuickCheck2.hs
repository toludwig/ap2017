{-
Example code produced during AP lecture.

Using QuickCheck to test the Morse module.  Second attempt, make a
special generator for strings only containing ASCII letter.  However,
the `listOf` combinator generates strings that are too long to be
practical (Take 1). Instead we use vectorOf to keep the length limited
(Take 2).

-}

import Test.QuickCheck
import qualified Data.Char as C
import qualified Morse


asciiLetter = elements (['a'..'z'] ++ ['A'..'Z'])

newtype LettersOnly = LO String
                    deriving (Eq, Show)



upper = map C.toUpper

prop_encode_decode (LO s) = (upper s) `elem` Morse.decode (Morse.encode s)

main = quickCheck prop_encode_decode


-- Take 2
instance Arbitrary LettersOnly where
  arbitrary = fmap LO $ do
    n <- choose (1, 6)
    vectorOf n asciiLetter

-- Take 1
-- instance Arbitrary LettersOnly where
--   arbitrary = fmap LO $ listOf asciiLetter
