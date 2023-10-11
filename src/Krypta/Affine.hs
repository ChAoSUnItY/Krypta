module Krypta.Affine (affineEncrypt, affineDecrypt) where

import Data.Char (toLower)
import Krypta.Internal

affineEncrypt :: String -> Int -> Int -> String
affineEncrypt msg a b =
  map (\x -> fromAlphaEnum $ mod26 $ toAlphaEnum x * a + b) streamMsg
  where
    streamMsg = map toLower msg

affineDecrypt :: String -> Int -> Int -> String
affineDecrypt msg a b =
  map (\x -> fromAlphaEnum $ mod26 $ (toAlphaEnum x - b) * inv) streamMsg
  where
    inv = head $ filter (\x -> mod (x * a) 26 == 1) [1 .. 26]
    streamMsg = map toLower msg
