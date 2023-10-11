module Krypta.Vignere (vigenereEncrypt, vigenereDecrpyt) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (toLower)
import Krypta.Internal (fromAlphaEnum, mod26, toAlphaEnum)

vigenere :: (Int -> Int -> Int) -> String -> String -> String
vigenere op msg key =
  map (fromAlphaEnum . mod26 . uncurry op . bimap toAlphaEnum toAlphaEnum) streamMsg
  where
    msgLen = length msg
    loweredMsg = map toLower msg
    loweredKey = map toLower key
    streamMsg = zip loweredMsg (take msgLen (cycle loweredKey))

vigenereEncrypt :: String -> String -> String
vigenereEncrypt = vigenere (+)

vigenereDecrpyt :: String -> String -> String
vigenereDecrpyt = vigenere (-)
