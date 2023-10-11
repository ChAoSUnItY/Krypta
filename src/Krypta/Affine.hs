module Krypta.Affine (affineEncrypt, affineDecrypt) where

import Data.Char (toLower)
import Krypta.Internal (fromAlphaEnum, mod26, toAlphaEnum)

affineEncrypt :: String -> Int -> Int -> String
affineEncrypt msg a b =
  map (fromAlphaEnum . mod26 . (+ b) . (* a) . toAlphaEnum) streamMsg
  where
    streamMsg = map toLower msg

affineDecrypt :: String -> Int -> Int -> String
affineDecrypt msg a b =
  map (fromAlphaEnum . mod26 . (* inv) . subtract b . toAlphaEnum) streamMsg
  where
    inv = head $ filter ((== 1) . mod26 . (* a)) [1 .. 26]
    streamMsg = map toLower msg
