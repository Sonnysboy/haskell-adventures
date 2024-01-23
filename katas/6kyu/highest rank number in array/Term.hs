module Term where
import Data.List
import Data.Function
import Control.Arrow


highestRank :: Ord c => [c] -> c
highestRank x = snd $ maximumBy (compare `on` fst) $ (map (length &&& head) . group . sort) x


