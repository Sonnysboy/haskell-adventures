

module LazyNext where
import Data.List ( elemIndex )

next :: Eq a => a -> [a] -> Maybe a
next item xs = case elemIndex item xs of
  Just index -> case compare (index + 1) $ length xs of
    EQ -> Nothing
    GT -> Nothing
    _ -> Just $ xs !! (index + 1)
  Nothing -> Nothing



--- top solution:
-- import Data.Maybe (listToMaybe)

-- next :: Eq a => a -> [a] -> Maybe a
-- next item = listToMaybe . drop 1 . dropWhile (/=item)
-- listToMaybe returns either Just (first element of list) or Nothing if the list was empty.