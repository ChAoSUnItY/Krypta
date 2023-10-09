module Krypta.Affine(affineEncrypt, affineDecrypt) where

import Krypta.Internal
import Data.Char (toLower)

affineEncrypt :: String -> Int -> Int -> String
affineEncrypt msg a b =
    map (\x -> fromAlphaEnum $ mod (toAlphaEnum x * a + b) 26) streamMsg
    where
        streamMsg = map toLower msg

affineDecrypt :: String -> Int -> Int -> String
affineDecrypt msg a b =
    map (\x -> fromAlphaEnum $ mod ((toAlphaEnum x - b) * inv) 26) streamMsg
    where
        inv = head $ filter (\x -> mod (x * a) 26 == 1) [1..26]
        streamMsg = map toLower msg
