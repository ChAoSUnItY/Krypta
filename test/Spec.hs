import Test.QuickCheck
import Krypta (vigenereEncrypt, vigenereDecrpyt)

data CipherPair = CipherPair String String
    deriving(Show)

instance Arbitrary CipherPair where
    arbitrary = do
        msg <- randomString
        CipherPair msg <$> randomString

main :: IO ()
main = vigenereTest

randomString :: Gen String
randomString = listOf1 $ choose ('a', 'z')

vigenereTest :: IO ()
vigenereTest = quickCheck ((\(CipherPair msg key) -> vigenereDecrpyt (vigenereEncrypt msg key) key == msg) :: CipherPair -> Bool)
