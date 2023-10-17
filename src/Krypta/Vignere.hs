{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Krypta.Vignere (vigenereEncrypt, vigenereDecrpyt) where

import Data.Modular ( type (/), ℤ )
import Krypta.Internal (fromStreamMsg, toStreamMsg)

vigenere :: (ℤ / 26 -> ℤ / 26 -> ℤ / 26) -> String -> String -> String
vigenere op msg key =
  fromStreamMsg $ map (uncurry op) streamMsg
  where
    msgLen = length msg
    streamMsg = zip (toStreamMsg msg) (toStreamMsg $ take msgLen (cycle key))

vigenereEncrypt :: String -> String -> String
vigenereEncrypt = vigenere (+)

vigenereDecrpyt :: String -> String -> String
vigenereDecrpyt = vigenere (-)
