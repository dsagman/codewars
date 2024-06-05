-- Write a class called User that is used to calculate the amount that a user will progress through a ranking system similar to the one Codewars uses.

-- Business Rules:

-- A user starts at rank -8 and can progress all the way to 8.
-- There is no 0 (zero) rank. The next rank after -1 is 1.
-- Users will complete activities. These activities also have ranks.
-- Each time the user completes a ranked activity the users rank progress is updated based off of the activity's rank
-- The progress earned from the completed activity is relative to what the user's current rank is compared to the rank of the activity
-- A user's rank progress starts off at zero, each time the progress reaches 100 the user's rank is upgraded to the next level
-- Any remaining progress earned while in the previous rank will be applied towards the next rank's progress (we don't throw any progress away).
-- The exception is if there is no other rank left to progress towards (Once you reach rank 8 there is no more progression).
-- A user cannot progress beyond rank 8.
-- The only acceptable range of rank values is -8,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8. Any other value should raise an error.
-- The progress is scored like so:

-- Completing an activity that is ranked the same as that of the user's will be worth 3 points
-- Completing an activity that is ranked one ranking lower than the user's will be worth 1 point
-- Any activities completed that are ranking 2 levels or more lower than the user's ranking will be ignored
-- Completing an activity ranked higher than the current user's rank will accelerate the rank progression.
-- The greater the difference between rankings the more the progression will be increased.
-- The formula is 10 * d * d where d equals the difference in ranking between the activity and the user.
-- Logic Examples:

-- If a user ranked -8 completes an activity ranked -7 they will receive 10 progress
-- If a user ranked -8 completes an activity ranked -6 they will receive 40 progress
-- If a user ranked -8 completes an activity ranked -5 they will receive 90 progress
-- If a user ranked -8 completes an activity ranked -4 they will receive 160 progress,
--      resulting in the user being upgraded to rank -7 and having earned 60 progress towards their next rank
-- If a user ranked -1 completes an activity ranked 1 they will receive 10 progress (remember, zero rank is ignored)
-- Code Usage Examples:

-- rank newUser -- => -8
-- progress newUser -- => 0
-- let u2 = incProgress (-7) newUser
-- progress u2 -- =>  10
-- let u3 = incProgress (-5) u2 -- will add 90 progress
-- progress u3 -- => 0 -- progress is now zero
-- rank u3 -- => -7 -- rank was upgraded to -7


import Control.Exception (evaluate)
import Test.Hspec ( describe, it, anyException, shouldBe, shouldThrow, Spec, hspec )
import Test.HUnit ( assertEqual )
import Data.Maybe (fromMaybe, fromJust, isNothing)

data Rank = Rn8 | Rn7 | Rn6 | Rn5 | Rn4 | Rn3 | Rn2 | Rn1 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
          deriving (Show, Ord, Eq, Enum, Bounded)

data User = User {trueRank :: Rank, progress :: Int}
          deriving (Show, Ord, Eq)

mkRank :: Int ->  Maybe Rank
mkRank = flip lookup (zip ([(-8),(-7)..(-1)]++[1..8]) [Rn8 .. R8])

rankToInt :: Rank -> Maybe Int
rankToInt = flip lookup (zip [Rn8 .. R8] ([(-8),(-7)..(-1)]++[1..8]))

newUser :: User
newUser = User {progress = 0, trueRank = Rn8}

incProgress :: Int -> User -> User
incProgress activity user
  | isNothing (mkRank activity) = error "Invalid activity number"
  | otherwise = user {progress = newProgress, trueRank = newRank}
  where
        pr = divMod (pValue activity (trueRank user) + progress user) 100
        newRank = iPlusR (fst pr) (trueRank user)
        newProgress = if newRank == R8 then 0 else snd pr

pValue :: Int -> Rank -> Int
pValue activity rank
  | isNothing d   = 0
  | rank == R8     = 0
  | d == Just (-1) = 1
  | d == Just 0    = 3
  | activity > fromJust (rankToInt rank)  = 10 * fromJust d * fromJust d
  | otherwise     = 0
  where d = iMinusR activity rank

iMinusR :: Int -> Rank -> Maybe Int
iMinusR i r = do
                  rankI <- mkRank i
                  return (fromEnum rankI - fromEnum r)

iPlusR :: Int -> Rank ->  Rank
iPlusR i r
    | rankPlus >= fromEnum R8  = R8
    | otherwise = toEnum rankPlus
    where rankPlus = fromEnum r + i

rank :: User -> Int
rank  = fromJust . rankToInt . trueRank

-- ---------------------------------------------------
-- Testing code
-- ---------------------------------------------------
data Result = R Int Int deriving (Eq)
instance Show Result where
  show (R r p) = concat ["rank=", show r, " progress=", show p]

doTest userBefore trank exRank exProgress = do
  let userAfter = incProgress trank userBefore
      expected = R exRank exProgress
      before = R (rank userBefore) (progress userBefore)
      actual = R (rank userAfter) (progress userAfter)
      errorMsg = unlines [ "User: " ++ show before
                         , "Task: rank=" ++ show trank
                         ]
  assertEqual errorMsg expected actual
  return userAfter

spec :: Spec
spec = do
  describe "User" $ do
    it "should start from beginning" $ do
      let user = newUser
          expected = R (-8) 0
          actual = R (rank user) (progress user)
      actual `shouldBe` expected

    it "should properly support upgrading to next level" $ do
      doTest newUser (-8) (-8) 3
      doTest newUser (-7) (-8) 10
      doTest newUser (-6) (-8) 40
      doTest newUser (-5) (-8) 90
      doTest newUser (-4) (-7) 60
      return ()

    it "should properly support upgrading multiple levels" $ do
      -- next one fails
      doTest newUser (-3) (-6) 50
      doTest newUser (-2) (-5) 60
      doTest newUser (-1) (-4) 90
      doTest newUser 1 (-2) 40
      doTest newUser 2 1 10
      doTest newUser 3 3 0
      doTest newUser 4 5 10
      doTest newUser 5 7 40
      doTest newUser 6 8 0
      doTest newUser 7 8 0
      doTest newUser 8 8 0
      return ()

    it "should properly support chain of tasks" $ do
      u <- doTest newUser 1 (-2) 40
      u <- doTest u 1 (-2) 80
      u <- doTest u 1 (-1) 20
      u <- doTest u 1 (-1) 30
      u <- doTest u 1 (-1) 40
      u <- doTest u 2 (-1) 80
      u <- doTest u 2 1 20
      u <- doTest u (-1) 1 21
      u <- doTest u 3 1 61
      u <- doTest u 8 6 51
      u <- doTest u 8 6 91
      u <- doTest u 8 7 31
      u <- doTest u 8 7 41
      u <- doTest u 8 7 51
      u <- doTest u 8 7 61
      u <- doTest u 8 7 71
      u <- doTest u 8 7 81
      u <- doTest u 8 7 91
      u <- doTest u 8 8 0
      u <- doTest u 8 8 0
      return ()

    it "should handle invalid range values" $ do
      evaluate (incProgress 9 newUser) `shouldThrow` anyException
      evaluate (incProgress 0 newUser) `shouldThrow` anyException
      evaluate (incProgress (-9) newUser) `shouldThrow` anyException





-- class RR a  where
--   r :: a -> Int

-- instance RR Rank where
-- r :: Rank -> Int
-- r Rn8 = -8
-- r Rn7 = -7
-- r Rn6 = -6
-- r Rn5 = -5
-- r Rn4 = -4
-- r Rn3 = -3
-- r Rn2 = -2
-- r Rn1 = -1
-- r R1  = 1
-- r R2  = 2
-- r R3  = 3
-- r R4  = 4
-- r R5  = 5
-- r R6  = 6
-- r R7  = 7
-- r R8  = 8

-- intToRank :: Int -> Maybe Rank
-- intToRank n
--   | n < 0  = Nothing
--   | n > 15 = Nothing
--   | otherwise = Just (toEnum n :: Rank)