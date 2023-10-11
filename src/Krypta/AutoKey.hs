module Krypta.AutoKey (autoKeyEncrypt, autoKeyDecrypt, decryptFullKey) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (toLower)
import Krypta.Internal (fromAlphaEnum, mod26, toAlphaEnum)

autoKey :: (Int -> Int -> Int) -> String -> String -> String
autoKey op msg key =
  map (fromAlphaEnum . mod26 . uncurry op . bimap toAlphaEnum toAlphaEnum) streamMsg
  where
    streamMsg = zip msg key

autoKeyEncrypt :: String -> String -> String
autoKeyEncrypt msg key =
  autoKey (+) msg fullKey
  where
    msgLen = length msg
    loweredMsg = map toLower msg
    loweredKey = map toLower key
    fullKey = take msgLen (loweredKey ++ cycle loweredMsg)

decryptFullKey :: String -> String -> String
decryptFullKey msg key = decryptFullKey' (length msg) (length key) msg key

decryptFullKey' :: Int -> Int -> String -> String -> String
decryptFullKey' target keyLen msg key
  | target > keyLen = key ++ decryptFullKey' (target - keyLen) keyLen (drop keyLen msg) streamingKey
  | otherwise = key
  where
    streamingKey = autoKey (-) msg key

autoKeyDecrypt :: String -> String -> String
autoKeyDecrypt msg key = autoKey (-) loweredMsg fullKey
  where
    loweredMsg = map toLower msg
    loweredKey = map toLower key
    fullKey = decryptFullKey loweredMsg loweredKey
