module Krypta.Internal(mod26, toAlphaEnum, fromAlphaEnum) where

mod26 :: Int -> Int
mod26 i = mod (i + 26) 26

toAlphaEnum :: Char -> Int
toAlphaEnum c = fromEnum c - 97

fromAlphaEnum :: Int -> Char
fromAlphaEnum i = toEnum $ i + 97
