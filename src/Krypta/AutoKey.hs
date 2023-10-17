{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Krypta.AutoKey (autoKeyEncrypt, autoKeyDecrypt, decryptFullKey) where

import Data.Char (isSpace, toLower)
import Data.Modular ( type (/), ℤ )
import Krypta.Internal (fromStreamMsg, toStreamMsg)

autoKey :: (ℤ / 26 -> ℤ / 26 -> ℤ / 26) -> String -> String -> String
autoKey op msg key =
  fromStreamMsg $ map (uncurry op) streamMsg
  where
    streamMsg = zip (toStreamMsg msg) (toStreamMsg key)

autoKeyEncrypt :: String -> String -> String
autoKeyEncrypt msg key =
  autoKey (+) msg fullKey
  where
    msgLen = length $ filter (not . isSpace) msg
    fullKey = take msgLen (key ++ cycle msg)

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
