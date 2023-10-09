import Krypta.Affine (affineDecrypt, affineEncrypt)
import Krypta.Vignere (vigenereDecrpyt, vigenereEncrypt)
import Test.QuickCheck

data VigenereCipherPair = VigenereCipherPair String String
  deriving (Show)

instance Arbitrary VigenereCipherPair where
  arbitrary = do
    msg <- randomString
    VigenereCipherPair msg <$> randomString

data AffineCipherPair = AffineCipherPair String Int Int
  deriving (Show)

instance Arbitrary AffineCipherPair where
  arbitrary = do
    msg <- randomString
    a <- elements $ filter (\x -> gcd x 26 == 1) [1 .. 25]
    AffineCipherPair msg a <$> suchThat (arbitrary :: Gen Int) (> 0)

main :: IO ()
main = do
  vigenereTest
  affineTest

randomString :: Gen String
randomString = listOf1 $ choose ('a', 'z')

vigenereTest :: IO ()
vigenereTest = quickCheck ((\(VigenereCipherPair msg key) -> vigenereDecrpyt (vigenereEncrypt msg key) key == msg) :: VigenereCipherPair -> Bool)

affineTest :: IO ()
affineTest = quickCheck ((\(AffineCipherPair msg a b) -> affineDecrypt (affineEncrypt msg a b) a b == msg) :: AffineCipherPair -> Bool)
