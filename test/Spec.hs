{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Data.Modular (toMod, ℤ, type (/))
import Krypta.Affine (affineDecrypt, affineEncrypt)
import Krypta.AutoKey (autoKeyDecrypt, autoKeyEncrypt)
import Krypta.Caesar (caesarDecrypt, caesarEncrypt)
import Krypta.Vignere (vigenereDecrpyt, vigenereEncrypt)
import Test.QuickCheck

newtype Coprime = Coprime (ℤ / 26)
  deriving (Show)

instance Arbitrary Coprime where
  arbitrary :: Gen Coprime
  arbitrary = Coprime <$> elements [1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25]

newtype Mod26 = Mod26 (ℤ / 26)
  deriving (Show)

instance Arbitrary Mod26 where
  arbitrary :: Gen Mod26
  arbitrary = Mod26 . (toMod @26) <$> arbitrary

newtype NonEmptyLowercase = NEL String
  deriving (Show)

instance Arbitrary NonEmptyLowercase where
  arbitrary :: Gen NonEmptyLowercase
  arbitrary = NEL <$> listOf1 (choose ('a', 'z'))

main :: IO ()
main = do
  quickCheck vigenereTest
  quickCheck affineTest
  quickCheck autoKeyTest
  quickCheck caesarTest

vigenereTest :: NonEmptyLowercase -> NonEmptyLowercase -> Bool
vigenereTest (NEL msg) (NEL key) = vigenereDecrpyt (vigenereEncrypt msg key) key == msg

affineTest :: NonEmptyLowercase -> Coprime -> Mod26 -> Bool
affineTest (NEL msg) (Coprime a) (Mod26 b) = affineDecrypt (affineEncrypt msg a b) a b == msg

autoKeyTest :: NonEmptyLowercase -> NonEmptyLowercase -> Bool
autoKeyTest (NEL msg) (NEL key) = autoKeyDecrypt (autoKeyEncrypt msg key) key == msg

caesarTest :: NonEmptyLowercase -> Mod26 -> Bool
caesarTest (NEL msg) (Mod26 offset) = caesarDecrypt (caesarEncrypt msg offset) offset == msg
