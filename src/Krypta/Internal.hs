{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Krypta.Internal (toStreamMsg, fromStreamMsg, toAlphaEnum, fromAlphaEnum) where

import Data.Char (isSpace, toLower)
import Data.Modular ( toMod, unMod, type (/), ℤ )

toStreamMsg :: String -> [ℤ / 26]
toStreamMsg = map (fromAlphaEnum . toLower) . filter (not . isSpace)

fromStreamMsg :: [ℤ / 26] -> String
fromStreamMsg = map toAlphaEnum

fromAlphaEnum :: Char -> ℤ / 26
fromAlphaEnum = toMod @26 . toInteger . subtract 97 . fromEnum

toAlphaEnum :: ℤ / 26 -> Char
toAlphaEnum = toEnum . (+ 97) . fromInteger . unMod
