module Krypta.AutoKey (autoKeyEncrypt, autoKeyDecrypt, autoKey) where

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

constructFullKey :: String -> String -> String
constructFullKey msg key
  | msgLen > keyLen = key ++ constructFullKey (drop keyLen msg) partialKey
  | otherwise = key
  where
    msgLen = length msg
    keyLen = length key
    streamKey = zip msg key
    partialKey = map (fromAlphaEnum . mod26 . uncurry (-) . bimap toAlphaEnum toAlphaEnum) streamKey

autoKeyDecrypt :: String -> String -> String
autoKeyDecrypt msg key = autoKey (-) loweredMsg fullKey
  where
    loweredMsg = map toLower msg
    loweredKey = map toLower key
    fullKey = constructFullKey msg loweredKey
