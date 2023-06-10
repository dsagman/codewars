
import Test.Hspec
import Criterion.Measurement (secs)

-- Write a function, which takes a non-negative integer (seconds)
-- as input and returns the time in a human-readable format (HH:MM:SS)

-- HH = hours, padded to 2 digits, range: 00 - 99
-- MM = minutes, padded to 2 digits, range: 00 - 59
-- SS = seconds, padded to 2 digits, range: 00 - 59
-- The maximum time never exceeds 359999 (99:59:59)

-- You can find some examples in the test fixtures.


humanReadable :: Int -> String
humanReadable x = make2digit (fst hrs) ++ ":" ++ make2digit (fst mins) ++ ":" ++ make2digit (snd mins)
                    where hrs = divMod x 3600
                          mins = divMod (snd hrs) 60

make2digit :: Int -> String
make2digit xs
    | length (show xs) == 1 = '0' : show xs
    | otherwise = show xs



spec :: IO ()
spec = hspec $ do
  describe "humanReadable" $ do
    it "should work for some examples" $ do
      humanReadable 0      `shouldBe` "00:00:00"
      humanReadable 59     `shouldBe` "00:00:59"
      humanReadable 60     `shouldBe` "00:01:00"
      humanReadable 90     `shouldBe` "00:01:30"
      humanReadable 86399  `shouldBe` "23:59:59"
      humanReadable 359999 `shouldBe` "99:59:59"