import Data.Char
import Data.Map (Map, (!))
import Data.List.Split
import qualified Data.Map as M
import Data.Tuple

import Test.Hspec ( hspec, describe, it, shouldBe )
import Control.Monad


-- spec :: IO ()
-- spec =hspec $ do
--   describe "decodeMorse" $ do
--     it "should work on the example from the description" $ do
--       decodeMorse ".... . -.--   .--- ..- -.. ." `shouldBe` "HEY JUDE"

test :: String
test = ".... . -.--   .--- ..- -.. ."

morseCodes :: M.Map String String -- This is given to us by the exercise
morseCodes = M.fromList [("....", "H"), (".", "E"), ("-.--", "Y"), (".---", "J"), ("..-", "U"), ("-..", "D")]

-- decodeMorse :: String -> String
decodeMorse = unwords . filter (not . null) . map ((morseCodes !) <=< words) . splitOn "   "



-- test = ".... . -.--   .--- ..- -.. ."

-- decodeMorse :: String -> String
-- decodeMorse = unwords . map (map (unmorseCode !) . words) . splitOn "   "

-- unmorseCode :: Map String Char
-- unmorseCode = (M.fromList . map swap . M.toList) morseCode

-- -- | Morse Code convert map.
-- morseCode :: Map Char String
-- morseCode = M.fromList
--   [('A', ".-")
--   ,('B', "-...")
--   ,('C', "-.-.")
--   ,('D', "-..")
--   ,('E', ".")
--   ,('F', "..-.")
--   ,('G', "--.")
--   ,('H', "....")
--   ,('I', "..")
--   ,('J', ".---")
--   ,('K', "-.-")
--   ,('L', ".-..")
--   ,('M', "--")
--   ,('N', "-.")
--   ,('O', "---")
--   ,('P', ".--.")
--   ,('Q', "--.-")
--   ,('R', ".-.")
--   ,('S', "...")
--   ,('T', "-")
--   ,('U', "..-")
--   ,('V', "...-")
--   ,('W', ".--")
--   ,('X', "-..-")
--   ,('Y', "-.--")
--   ,('Z', "--..")
--   ,('=', "-...-")
--   ,('?', "..--..")
--   ,('/', "-..-.")
--   ,(',', "--..--")
--   ,('.', ".-.-.-")
--   ,(':', "---...")
--   ,('\'', ".----.")
--   ,('-', "-....-")
--   ,('(', "-.--.")
--   ,(')', "-.--.-")
--   ,('0', "-----")
--   ,('1', ".----")
--   ,('2', "..---")
--   ,('3', "...--")
--   ,('4', "....-")
--   ,('5', ".....")
--   ,('6', "-....")
--   ,('7', "--...")
--   ,('8', "---..")
--   ,('9', "----.")
--   ,('@', ".--.-.")]