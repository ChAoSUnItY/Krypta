{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Krypta.Caesar (caesarEncrypt, caesarDecrypt) where

import Data.Modular (ℤ, type (/))
import Krypta.Internal (fromStreamMsg, toStreamMsg)

caesarEncrypt :: String -> ℤ / 26 -> String
caesarEncrypt msg offset = fromStreamMsg $ map (+ offset) $ toStreamMsg msg

caesarDecrypt :: String -> ℤ / 26 -> String
caesarDecrypt msg offset = fromStreamMsg $ map (subtract offset) $ toStreamMsg msg
