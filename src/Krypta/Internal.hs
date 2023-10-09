module Krypta.Internal where

toAlphaEnum :: Char -> Int
toAlphaEnum c = fromEnum c - 97

fromAlphaEnum :: Int -> Char
fromAlphaEnum i = toEnum $ i + 97
