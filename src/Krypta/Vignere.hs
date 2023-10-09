module Krypta.Vignere (vigenereEncrypt, vigenereDecrpyt) where

import Data.Char (toLower)
import Krypta.Internal (fromAlphaEnum, toAlphaEnum)

-- Zips up first and second list, if the second list is not
-- enough to zip up with the first one, then cycles it until
-- both lists' length are same.
zipReplicate :: [a] -> [b] -> [(a, b)]
zipReplicate a' b' = zip a' (take (length a') (cycle b'))

vigenere :: (Int -> Int -> Int) -> String -> String -> String
vigenere op msg key =
  map (\x -> fromAlphaEnum $ mod (op (toAlphaEnum $ fst x) (toAlphaEnum $ snd x)) 26) streamMsg
  where
    streamMsg = zipReplicate (map toLower msg) (map toLower key)

vigenereEncrypt :: String -> String -> String
vigenereEncrypt = vigenere (+)

vigenereDecrpyt :: String -> String -> String
vigenereDecrpyt = vigenere (-)
