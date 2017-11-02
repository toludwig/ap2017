{-
Example code produced "during" AP lecture.

Using QuickCheck to test the Morse module.

Take 0: Found bug Morse module (fixed)
Take 1: Forgot to take lowercase letter into account
Take 2: Forgot to take non-letter and non-ascii into account
Take 3: Use precondition

Author: Ken Friis Larsen <kflarsen@diku.dk>
-}

import Test.QuickCheck
import qualified Morse
import qualified Data.Char as C


-- Take 1
prop_encode_decode_t1 s = s `elem` Morse.decode (Morse.encode s)

-- Take 2
prop_encode_decode_t2 s = (fmap C.toUpper s) `elem` Morse.decode (Morse.encode s)

-- Take 3
prop_encode_decode s =
  good s ==> (fmap C.toUpper s) `elem` Morse.decode (Morse.encode s)

main = quickCheck prop_encode_decode

upper = map C.toUpper
good = all $ \c -> C.isAscii c && C.isLetter c


-- version including classification of input
prop_encode_decode_with_classify s =
  classify (null s) "empty string" $
  good s ==> upper s `elem` Morse.decode (Morse.encode s)
