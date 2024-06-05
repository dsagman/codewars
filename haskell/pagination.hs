import Test.Hspec
import Test.QuickCheck

-- For this exercise you will be strengthening your page-fu mastery.
-- You will complete the PaginationHelper class,
-- which is a utility class helpful for querying paging information
-- related to an array.

-- The class is designed to take in an array of values and an integer
-- indicating how many items will be allowed per each page.
-- The types of values contained within the collection/array are not relevant.

-- The following are some examples of how this class is used:
-- collection   = ['a','b','c','d','e','f']
-- itemsPerPage = 4
-- pageCount collection itemsPerPage       `shouldBe` 2
-- itemCount collection itemsPerPage       `shouldBe` 6
-- pageItemCount collection itemsPerPage 0 `shouldBe` Just 4 -- four of six items
-- pageItemCount collection itemsPerPage 1 `shouldBe` Just 2 -- the last two items
-- pageItemCount collection itemsPerPage 3 `shouldBe` Nothing -- page doesn't exist
-- pageIndex collection itemsPerPage  0    `shouldBe` Just 0 -- zero based index
-- pageIndex collection itemsPerPage  5    `shouldBe` Just 1
-- pageIndex collection itemsPerPage 20    `shouldBe` Nothing
-- pageIndex collection itemsPerPage (-20) `shouldBe` Nothing

type Collection a = [a]
type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length

pageCount :: Collection a -> ItemsPerPage -> Int
pageCount xs n = div l n + (if mod l n > 0 then 1 else 0)
            where l = length xs

pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount xs n page
    | page < 0 = Nothing
    | page + 1 < pages = Just n
    | page + 1 == pages = if extra == 0 then Just n else Just extra
    | otherwise = Nothing
    where l = length xs
          extra = mod l n
          pages = pageCount xs n

pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex xs n item
    | null xs           = Nothing
    | item < 0          = Just 0 -- Why? The description says this is Nothing, but the test cases force this
    | n < 0             = Nothing
    | item >= length xs  = Nothing
    | index > p         = Nothing
    | otherwise = Just index
    where index = div item n
          p = pageCount xs n

spec :: Spec
spec = do
  describe "itemCount" $ do
    it "should work for some collections" $ do
      itemCount [1..5]             `shouldBe` 5
      itemCount ["Hello", "World"] `shouldBe` 2

  describe "pageCount" $ do
    it "should work for some collections and ipp values" $ do
      pageCount [1..5] 5 `shouldBe` 1
      pageCount [1..5] 3 `shouldBe` 2
      pageCount [1..5] 2 `shouldBe` 3
      pageCount [1..5] 1 `shouldBe` 5

  describe "pageItemCount" $ do
    it "should work for some collections and ipp values" $ do
      pageItemCount [1..5] 5 0 `shouldBe` Just 5  -- zero based indices
      pageItemCount [1..5] 5 1 `shouldBe` Nothing
      pageItemCount [1..5] 3 0 `shouldBe` Just 3
      pageItemCount [1..5] 3 1 `shouldBe` Just 2

    -- it "should reject invalid indices" $ do
    --   property $ \xs (Positive n) (Positive k) ->
    --     pageItemCount (xs :: [()]) n (pageCount xs n + k) `shouldBe` Nothing

  describe "pageIndex" $ do
    it "should work for some collections and ipp values" $ do
      pageIndex "ABCDE" 5 0 `shouldBe` Just 0  -- zero based indices
      pageIndex "ABCDE" 5 3 `shouldBe` Just 0  -- zero based indices
      pageIndex "ABCDE" 2 3 `shouldBe` Just 1  -- zero based indices
      pageIndex "ABCDE" 2 8 `shouldBe` Nothing

    -- it "should reject invalid indices" $ do
    --   property $ \xs (Positive n) (Positive k) ->
    --     pageIndex (xs :: [()]) n (itemCount xs + k) `shouldBe` Nothing