{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Krypta.Affine (affineEncrypt, affineDecrypt) where

import Data.Modular ( unMod, type (/), ℤ )
import Krypta.Internal (fromStreamMsg, toStreamMsg)

affineEncrypt :: String -> ℤ / 26 -> ℤ / 26 -> String
affineEncrypt msg a b =
  fromStreamMsg $ map ((+ b) . (* a)) $ toStreamMsg msg

affineDecrypt :: String -> ℤ / 26 -> ℤ / 26 -> String
affineDecrypt msg a b =
  fromStreamMsg $ map ((* inverse) . subtract b) $ toStreamMsg msg
  where
    inverse = head $ filter ((== 1) . unMod . (* a)) [1 .. 25]
